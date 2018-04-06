const acorn = require('acorn');

const int = '([+\\-]?\\d+)';
const item = `(\\[${int}?(:${int}?)?\\])?${int}?`;
const exp = `(${item}(, *${item})*)`;
const def = `([a-zA-Z])\\t+(?:([^\\t]+)\\t+)?`;
const ChordItem = new RegExp(`${item}`);
const ChordPatt = new RegExp(`^${def}${exp}$`);

const AliasPatt = /^(?: *prec(?:edence)?:(\d+);) *alias:(.+)$/i;

class LibTokenizer {

  static ChordTokenize(lines) {
    const data = [], warnings = [];
    lines.forEach(line => {
      const match = line.match(ChordPatt);
      if (match) {
        const notation = match[1];
        const comment = match[2];
        const pitches = match[3].split(/, */).map(item => {
          const data = item.match(ChordItem);
          return [
            data[2] ? parseInt(data[2]) : 0,
            data[4] ? parseInt(data[4]) :
            data[3] ? -1 :
            data[2] ? parseInt(data[2]) : 
            data[1] ? -1 : 0,
            data[5] ? parseInt(data[5]) : 0
          ];
        });
        data.push({
          Notation: notation,
          Comment: comment,
          Pitches: pitches
        });
      } else {
        if (!line.match(/^\s*$/)) {
          warnings.push({
            Err: 'InvChordDecl',
            Data: line
          });
        }
      }
    });
    return {
      Data: data,
      Warnings: warnings
    };
  }
  
  static FunctionTokenize(code) {
    const alias = [], data = [], warnings = [];
    const result = acorn.parse(code, {
      ecmaVersion: 8,
      onComment(isBlock, text, start, end) {
        const result = AliasPatt.exec(text);
        if (!isBlock && result) {
          let prec = result[1];
          if (!prec) {
            // Set default precedence
            prec = 100;
          } else {
            prec = parseInt(prec.trim());
          }
          alias.push({
            prec: prec, 
            stx: result[2].trim(), 
            start: start, 
            end: end 
          });
        }
      }
    });
    result.body.forEach((tok) => {
      if (tok.type === 'FunctionDeclaration') {
        const info = {
          Name: tok.id.name,
          Alias: []
        };
        let i = alias.length;
        while (i--) {
          if (tok.body.start < alias[i].start && tok.body.end > alias[i].end) {
            info.Alias.push({
              Prec: alias[i].prec,
              Syntax: alias[i].stx
            });
            alias.splice(i, 1);
          }
        }
        data.push(info);
      } else {
        warnings.push({
          Err: 'NotFuncDecl',
          Type: tok.type,
          Start: tok.start,
          End: tok.end
        });
      }
    });
    return {
      Data: data,
      Warnings: warnings
    };
  }

}

module.exports = LibTokenizer;

