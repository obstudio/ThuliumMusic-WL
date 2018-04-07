const fs = require('fs');
const LibTokenizer = require('./Library');
const TrackSyntax = require('./Track');

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

    let source;
    if (spec === 'URL') {
      source = fs.readFileSync(input, "utf8");  
    } else {
      source = input;
    }
    this.Source = source.split(/\r?\n/g);
  }

  initialize() {
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
    let name, inst = [];
    const aliases = this.Function;
    const chords = this.Chord.map(chord => chord.Notation);
    return new TrackSyntax(aliases, ['1', '2', '3', '4', '5', '6', '7'], chords);
  }

  tokenizeSection(tracks, comments) {
    console.log(111);
    const src = tracks.map(this.tokenizeTrack);
    return {
      Tracks: tracks,
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

//throw 0;

// const test = new Tokenizer('../../Songs/test.tm', 'URL');
const test = new Tokenizer('../../package/Ammonia/main.tml', 'URL');
test.initialize();

console.log(test.tokenize().Sections);

