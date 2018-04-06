const fs = require('fs');
const LibTokenizer = require('./Library');
const TrackSyntax = require('./Track');

const packagePath = '../../package/';
const packageInfo = require(packagePath + 'index.json');

class Tokenizer {
  constructor(input, src = 'String') {
    this.Comment = [];
    this.Library = [];
    this.Warnings = [];
    this.Settings = [];
    this.Syntax = {
      Function: [],
      Chord: []
    };

    let source;
    if (src === 'URL') {
      source = fs.readFileSync(input, "utf8");  
    } else {
      source = input;
    }
    this.source = source.split(/\r?\n/g);
  }

  initialize() {
    let ptr = 0;
    const src = this.source;

    // Comments
    while (this.identify(ptr, '//')) {
      this.Comment.push(src[ptr].slice(2));
      ptr++;
    }

    // Libraries
    while (this.identify(ptr, '#')) {
      const origin = src[ptr];
      const command = origin.match(/[a-zA-Z]+/);
      ptr++;
      if (!command) continue;
      switch (command[0].toLowerCase()) {

        case 'include':
          const name = origin.slice(command.index + command[0].length).trim();
          if (name in packageInfo.Packages) {
            const packageData = new Tokenizer(packagePath + name + '/main.tml');
            this.FuncStx.push(...packageData.FuncStx);
            this.Chord.push(...packageData.Chord);
            this.Library.push({
              Type: 'Package',
              Path: name,
              Head: origin
            });
          } else {
            //
          }
          break;

        case 'chord':
          const lines = [];
          while (ptr < src.length && !src[ptr].startsWith('#')) {
            lines.push(src[ptr]);
            ptr++;
          }
          this.mergeLibrary(origin, lines, 'Chord');
          break;

        case 'function':
          let code = '';
          while (ptr < src.length && !src[ptr].startsWith('#')) {
            code += src[ptr] + '\n';
            ptr++;
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

  tokenize() {
    this.initialize();
    return {
      Comment: this.Comment,
      Library: this.Library,
      Score: this.Score
    };
  }

  identify(ptr, match) {
    return ptr < this.source.length && (
      this.source[ptr].startsWith(match) || 
      this.source[ptr].match(/^\s*$/)
    );
  }

  mergeLibrary(head, source, type) {
    const data = LibTokenizer[type + 'Tokenize'](source);
    this.Syntax[type].push(...data.Data);
    this.Warnings.push(...data.Warnings);
    this.Library.push({
      Type: type,
      Code: source,
      Head: head
    });
  }

}

module.exports = Tokenizer

