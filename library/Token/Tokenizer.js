const fs = require('fs');
const LibTokenizer = require('./Library');
const TrackSyntax = require('./Track');
const FSM = require('./Context');

const instrDict = require('../Config/Instrument.json');
const drumDict = require('../Config/Percussion.json');

const instrList = Object.keys(instrDict);
const drumList = Object.keys(drumDict);

const packagePath = '../../package/';
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

  constructor(input, spec = 'String') {
    this.Comment = [];
    this.Library = [];
    this.Warnings = [];
    this.Settings = [];
    this.Function = [];
    this.Chord = [];
    this.$init = false;

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
            const path = packagePath + name + '/main.tml';
            const packageData = new Tokenizer(path, 'URL').getLibrary();
            this.Function.push(...packageData.Function);
            this.Chord.push(...packageData.Chord);
            this.Library.push({
              Type: 'Package',
              Path: name,
              Head: origin
            });
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
          throw new Error();
          break;

      }
    }
    this.Score = src.slice(ptr);
    this.$init = true;
  }

  getLibrary() {
    this.initialize();
    return {
      Function: this.Function,
      Chord: this.Chord
    };
  }

  tokenize() {
    this.initialize();

    const sections = [];
    const src = this.Score;
    let ptr = 0, blank = 0;
    let tracks = [];
    let comments = [];

    while(ptr < src.length) {
      if (Tokenizer.startsTrue(src, ptr, '//')) {
        blank += 1;
        if (blank >= 2 && tracks.length != 0) {
          sections.push(this.tokenizeSection(tracks, comments));
          comments = [];
          tracks = [];
        }
        if (src[ptr].startsWith('//')) {
          comments.push(src[ptr].slice(2));
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
      sections.push(this.tokenizeSection(tracks, comments));
    }

    return {
      Comment: this.Comment,
      Library: this.Library,
      Settings: this.Settings,
      Sections: sections
    };
  }

  tokenizeTrack(track) {
    let name, play, inst = [], degrees = ['0', '%'];
    const instrDegrees = ['1', '2', '3', '4', '5', '6', '7'];
    const drumDegrees = ['x'];
    const aliases = this.Function;
    const chords = this.Chord.map(chord => chord.Notation);
    const meta = track.match(/^<(?:(:)?([a-zA-Z][a-zA-Z\d]*):)?/);
    if (meta) {
      play = meta[1];
      name = meta[2];
      const syntax = new TrackSyntax(aliases, degrees, chords);
      track = track.slice(meta[0].length);
      const data = syntax.tokenize(track, 'meta');
      data.Content.forEach(tok => {
        if (tok.Type != '@inst') {
          throw new Error();
          return;
        }
        if (instrList.includes(tok.name)) {
          instrDegrees.forEach(deg => {
            if (!degrees.includes(deg)) degrees.push(deg);
          });
        } else if (instrList.includes(tok.name)) {
          drumDegrees.forEach(deg => {
            if (!degrees.includes(deg)) degrees.push(deg);
          });
        } else {
          throw new Error();
          return;
        }
        inst.push({ Name: tok.name, Spec: tok.spec });
      });
      track = track.slice(data.Index);
    } else {
      degrees = ['1', '2', '3', '4', '5', '6', '7', '0', '%'];
    }
    const syntax = new TrackSyntax(aliases, degrees, chords);
    const result = syntax.tokenize(track, 'default');
    return {
      Play: Boolean(play),
      Name: name,
      Instruments: inst,
      Content: result.Content
    };
  }

  tokenizeSection(tracks, comments) {
    const result = tracks.map(track => this.tokenizeTrack(track));
    return {
      Tracks: result,
      Comments: comments
    };
  }

  mergeLibrary(head, source, type) {
    const data = LibTokenizer[type + 'Tokenize'](source);
    this[type].push(...data.Data);
    this.Warnings.push(...data.Warnings);
    this.Library.push({
      Type: type,
      Code: source,
      Head: head
    });
  }
}

module.exports = Tokenizer

// throw 0;

const test = new Tokenizer('../../Songs/test.tm', 'URL');

//test.tokenize()

console.log(test.tokenize().Sections[0].Tracks[0]);
