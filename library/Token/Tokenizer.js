const fs = require('fs');
const LibTokenizer = require('./Library');
const TrackSyntax = require('./Track');
const FSM = require('./Context');

const instrDict = require('../Config/Instrument.json');
const drumDict = require('../Config/Percussion.json');

const instrList = Object.keys(instrDict);
const drumList = Object.keys(drumDict);

const packagePath = __dirname + '/../../package/'
const packageInfo = require(packagePath + 'index.json');

class Tokenizer {
  static startsTrue(src, ptr, match, blank = true) {
    return ptr < src.length && (
      src[ptr].startsWith(match) || 
      src[ptr].match(/^\s*$/) && blank
    );
  }

  static startsFalse(src, ptr, match, blank = false) {
    return ptr < src.length && !(
      src[ptr].startsWith(match) ||
      src[ptr].match(/^\s*$/) && blank
    );
  }

  constructor(input, {spec = 'String', buffer = true} = {}) {
    this.Comment = [];
    this.Library = [];
    this.Warnings = [];
    this.Errors = [];
    this.Settings = [];

    this.Syntax = {
      Code: '',
      Dict: [],       // Function Attributes
      Alias: [],      // Function Aliases
      Chord: []       // Chord Operators
    };

    this.$init = false;
    this.$token = false;
    this.$buffer = buffer;

    let source;
    if (spec === 'URL') {
      source = fs.readFileSync(input, "utf8");  
    } else {
      source = input;
    }
    this.Source = source.split(/\r?\n/g);
  }

  initialize() {
    if (this.$init) return;

    let ptr = 0;
    const src = this.Source;

    // Comments
    while (Tokenizer.startsTrue(src, ptr, '//', false)) {
      this.Comment.push(src[ptr].slice(2));
      ptr += 1;
    }

    // Libraries
    while (Tokenizer.startsTrue(src, ptr, '#')) {
      const origin = src[ptr];
      const command = origin.match(/[a-zA-Z]+/);
      ptr += 1;
      if (!command) continue;
      switch (command[0].toLowerCase()) {

        case 'include':
          const name = origin.slice(command.index + command[0].length).trim();
          if (packageInfo.Packages.includes(name)) {
            this.loadLibrary(name, origin);
          } else {
            // custom
          }
          break;

        case 'chord':
          const lines = [];
          while (Tokenizer.startsFalse(src, ptr, '#')) {
            lines.push(src[ptr]);
            ptr += 1;
          }
          this.mergeLibrary(origin, lines, 'Chord');
          break;

        case 'function':
          let code = '';
          while (Tokenizer.startsFalse(src, ptr, '#')) {
            code += src[ptr] + '\n';
            ptr += 1;
          }
          this.mergeLibrary(origin, code, 'Function');
          break;

        case 'end':
          break;

        default:
          this.Errors.push({
            Err: 'InvaildCommand',
            Pos: ptr,
            Src: origin
          });
          break;

      }
    }
    this.Score = src.slice(ptr);
    this.$init = true;
  }

  tokenize() {
    if (this.$token) return;
    this.initialize();
    this.loadLibrary(packageInfo.AutoLoad);
    this.Sections = [];

    const src = this.Score;
    let ptr = 0, blank = 0;
    let tracks = [];
    let comment = [];

    while(ptr < src.length) {
      if (Tokenizer.startsTrue(src, ptr, '//')) {
        blank += 1;
        if (blank >= 2 && tracks.length != 0) {
          this.Sections.push(this.tokenizeSection(tracks, comment));
          comment = [];
          tracks = [];
        }
        if (src[ptr].startsWith('//')) {
          comment.push(src[ptr].slice(2));
        }
        ptr += 1;
      } else {
        let code = src[ptr];
        ptr += 1;
        while (Tokenizer.startsFalse(src, ptr, '//', true)) {
          code += '\n' + src[ptr];
          ptr += 1;
        }
        blank = 0;
        tracks.push(code);
      }
    }
    if (tracks.length != 0) {
      this.Sections.push(this.tokenizeSection(tracks, comment));
    }

    this.$token = true;
  }

  tokenizeTrack(track) {
    let name, play = true, inst = [], degrees = ['0', '%'];
    const instrDegrees = ['1', '2', '3', '4', '5', '6', '7'];
    const drumDegrees = ['x'];
    const functions = this.Syntax.Dict;
    const aliases = this.Syntax.Alias;
    const chords = this.Syntax.Chord.map(chord => chord.Notation);
    const meta = track.match(/^<(?:(:)?([a-zA-Z][a-zA-Z\d]*):)?/);

    if (meta) {
      play = !meta[1];
      name = meta[2];
      const syntax = new TrackSyntax(functions, aliases, chords, degrees);
      track = track.slice(meta[0].length);
      const data = syntax.tokenize(track, 'meta');
      data.Content.forEach(tok => {
        if (tok.Type != '@inst') {
          this.Warnings.push({
            Err: 'NotInstrument',
            Tok: tok
          });
        } else if (instrList.includes(tok.name)) {
          instrDegrees.forEach(deg => {
            if (!degrees.includes(deg)) degrees.push(deg);
          });
          inst.push({ Name: tok.name, Spec: tok.spec });
        } else if (drumList.includes(tok.name)) {
          drumDegrees.forEach(deg => {
            if (!degrees.includes(deg)) degrees.push(deg);
          });
          inst.push({ Name: tok.name, Spec: tok.spec });
        } else {
          this.Warnings.push({
            Err: 'NotInstrument',
            Tok: tok
          });
        }
      });
      track = track.slice(data.Index);
    }

    if (degrees.length === 2) {
      degrees = ['1', '2', '3', '4', '5', '6', '7', '0', '%'];
    }
    const syntax = new TrackSyntax(functions, aliases, chords, degrees);
    const result = syntax.tokenize(track, 'default');

    return {
      Play: play,
      Name: name,
      Instruments: inst,
      Content: result.Content,
      Warnings: result.Warnings
    };
  }

  tokenizeSection(tracklist, comment) {
    const result = tracklist.map(track => this.tokenizeTrack(track));
    const prolog = [], epilog = [], settings = [], tracks = [];
    
    result.forEach((track, index) => {
      const content = track.Content;
      if (content.every(tok => !FSM.isSubtrack(tok))) {
        let sep = content.findIndex(tok => tok.Type === 'LocalIndicator');
        if (index === 0 || index === result.length - 1) {
          if (sep === -1) sep = content.length;
          if (index === 0) {
            prolog.push(...content.slice(0, sep));
          } else {
            epilog.push(...content.slice(0, sep));
          }
        } else {
          sep = 0;
        }
        settings.push({
          Index: index,
          Spec: content.slice(sep)
        });
      } else {
        tracks.push(track);
      }
    });
    
    return {
      Prolog: prolog,
      Comment: comment,
      Settings: settings,
      Tracks: tracks,
      Epilog: epilog
    };
  }

  properties(...properties) {
    this.tokenize();
    const result = {};
    properties.forEach(attr => result[attr] = this[attr]);
    return result;
  }

  toParser() {
    return this.properties('Settings', 'Syntax', 'Sections');
  }

  fullForm() {
    return this.properties('Comment', 'Library', 'Settings', 'Warnings', 'Errors', 'Syntax', 'Sections');
  }

  getLibrary() {
    this.initialize();
    if (this.Errors.length > 0) {
      console.log(this.Errors); 
      throw 0;
    }
    return this.Syntax;
  }

  loadLibrary(name, origin = '#AUTOLOAD') {
    const path = packagePath + name;
    let packageData;
    if (this.$buffer && fs.existsSync(path + '/buffer.json')) {
      packageData = require(path + '/buffer.json');
    } else {
      packageData = new Tokenizer(path + '/main.tml', {spec: 'URL'}).getLibrary();
      // console.log(packageData);
      fs.writeFileSync(path + '/buffer.json', JSON.stringify(packageData), 'utf8');
    }
    this.Syntax.Dict.push(...packageData.Dict);
    this.Syntax.Chord.push(...packageData.Chord);
    this.Syntax.Alias.push(...packageData.Alias);
    this.Syntax.Code += packageData.Code;
    this.Library.push({
      Type: 'Package',
      Path: name,
      Head: origin
    });
  }

  mergeLibrary(head, source, type) {
    const data = LibTokenizer[type + 'Tokenize'](source);
    if (data.Chord) this.Syntax.Chord.push(...data.Chord);
    if (data.Dict) this.Syntax.Dict.push(...data.Dict);
    if (data.Alias) this.Syntax.Alias.push(...data.Alias);
    if (data.Code) this.Syntax.Code += data.Code;
    this.Errors.push(...data.Errors);
    this.Warnings.push(...data.Warnings);
    this.Library.push({
      Type: type,
      Code: source,
      Head: head
    });
  }
}

module.exports = Tokenizer

