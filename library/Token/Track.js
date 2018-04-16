const FSM = require('./Context');
const {Alias} = require('./Alias');

const ArgumentPatterns = {
  uns: {
    patt: '(\\d+(?:\\.\\d+)?)',
    meta: 'Expression'
  },
  sig: {
    patt: '([+\\-]\\d+(?:\\.\\d+)?)',
    meta: 'Expression'
  },
  int: {
    patt: '([+\\-]?\\d+(?:\\.\\d+)?)',
    meta: 'Expression'
  },
  exp: {
    patt: '([+\\-]?\\d+(?:[./]\\d+)?|Log2\\(\\d+\\)(?:[+\\-]\\d+)?)',
    meta: 'Expression'
  },
  str: {
    patt: '((?:[^\\{\\}\\(\\)\\[\\]\\"\\,]|\\\\.)*)',
    meta: 'String'
  },
  nam: {
    patt: '([a-zA-Z][a-zA-Z\\d]*)',
    meta: 'String'
  },
  mac: {
    patt: '(@[a-zA-Z]\\w*)',
    meta: 'Macrotrack'
  }
};

class NoteSyntax {
  constructor(chords, degrees) {
    const degree = NoteSyntax.ArrayToRegex(degrees, false);
    const chord = NoteSyntax.ArrayToRegex(chords, true);
    const pitOp = "[#b',]*";
    const durOp = '[._=-]*'
    const volOp = '[>:]*';
    const epilog = '[`]*';
    const inner = `(?:${pitOp}${chord}${volOp})`;
    const outer = `(?:${durOp}${epilog})`;
    this.deg = `(${degree})`;
    this.in = `(${pitOp})(${chord})(${volOp})`,
    this.out = `(${durOp})(${epilog})`;
    this.sqr = `\\[((?:${degree}${inner})+)\\]`;
    this.Patt = `((?:\\[(?:${degree}${inner})+\\]|${degree})${inner}${outer})`;
  }

  static ArrayToRegex(array, multi = true) {
    let charset = '', quantifier = '';
    if (array.length > 0) {
      if (multi) quantifier = '*';
      charset = '[' + array.join('') + ']';
    }
    return charset + quantifier;
  }
}

class TrackSyntax extends FSM {
  constructor(functions, aliases, chords, degrees) {
    const name = functions.map(func => func.Name).join('|');
    const note = new NoteSyntax(chords, degrees);
    const dict = Object.assign({
      not: {
        patt: note.Patt,
        meta: 'Subtrack',
        epilog: arg => this.tokenize(arg, 'note').Content
      }
    }, ArgumentPatterns);

    super({

      // Subtrack & Macrotrack & PlainFunction
      prototype: [
        {
          patt: /^\{(?:(\d+)\*)?/,
          push: 'default',
          token(match, content) {
            let repeat;
            if (match[1] != undefined) {
              repeat = parseInt(match[1]);
            } else {
              const volta = content.filter(tok => tok.Type === 'BarLine' && tok.Order[0] > 0);
              repeat = Math.max(-1, ...volta.map(tok => Math.max(...tok.Order)));
            }
            return {
              Type: 'Subtrack',
              Repeat: repeat,
              Content: content
            };
          }
        },
        {
          patt: new RegExp(`^(${name})\\(`),
          push: 'argument',
          token(match, content) {
            return {
              Type: 'Function',
              Name: match[1],
              Alias: -1,
              Args: content,
              VoidQ: functions.find(func => func.Name === match[1]).VoidQ
            };
          }
        },
        {
          patt: /^@([a-zA-Z]\w*)/,
          token(match) {
            return {
              Type: 'Macrotrack',
              Name: match[1]
            };
          }
        }
      ],

      note: [
        {
          patt: new RegExp('^' + note.deg + note.in + note.out),
          token(match) {
            return {
              Type: 'Note',
              Pitches: [
                {
                  Degree: match[1],
                  PitOp: match[2],
                  Chord: match[3],
                  VolOp: match[4]
                }
              ],
              PitOp: '',
              Chord: '',
              VolOp: '',
              DurOp: match[5],
              Stac: match[6].length
            };
          }
        },
        {
          patt: new RegExp('^' + note.sqr + note.in + note.out),
          token(match) {
            const inner = new RegExp(note.deg + note.in);
            const match1 = match[1].match(new RegExp(inner, 'g'));
            return {
              Type: 'Note',
              Pitches: match1.map(str => {
                const match = inner.exec(str);
                return {
                  Degree: match[1],
                  PitOp: match[2],
                  Chord: match[3],
                  VolOp: match[4]
                };
              }),
              PitOp: match[2],
              Chord: match[3],
              VolOp: match[4],
              DurOp: match[5],
              Stac: match[6].length
            };
          }
        }
      ],

      meta: [
        {
          patt: /^>/,
          pop: true
        },
        {
          patt: /([a-zA-Z][a-zA-Z\d]*)/,
          push: FSM.next('default', /^(?=>)/, /^,\s*/),
          token(match, content) {
            return {
              Type: '@inst',
              name: match[1],
              spec: content
            };
          }
        }
      ],

      // Track Contents
      default: [
        FSM.include('alias'),
        FSM.include('prototype'),
        FSM.include('note'),
        {
          patt: /^\}/,
          pop: true
        },
        {
          patt: new RegExp(`^\\((${name}):`),
          push: 'argument',
          token(match, content) {
            return {
              Type: 'Function',
              Name: match[1],
              Alias: 0,
              Args: content
            };
          }
        },
        FSM.item('RepeatEndBegin', /^:\|\|:/),
        FSM.item('RepeatBegin', /^\|\|:/),
        FSM.item('RepeatEnd', /^:\|\|/),
        FSM.item('LocalIndicator', /^!/),
        {
          patt: /^\[(?=(\d+(~\d+)\. *)+\])/,
          push: FSM.next('volta', /^\]/),
          token(match, content) {
            return {
              Type: 'volta',
              Order: [].concat(...content)
            };
          }
        },
        {
          patt: /^\\(?=(\d+(~\d+)?(, *\d+(~\d+)?)*)?:)/,
          push: FSM.next('volta', /^:/),
          token(match, content) {
            return {
              Type: 'BarLine',
              Skip: false,
              Overlay: false,
              Order: [].concat(...content)
            };
          },
          locate: false
        },
        {
          patt: /^(\/|\||\\)/,
          token(match) {
            return {
              Type: 'BarLine',
              Skip: match[0] === '\\',
              Overlay: match[0] === '/',
              Order: [0]
            };
          }
        },
        {
          patt: /^<\*/,
          push: [
            {
              patt: /^\*>/,
              pop: true
            },
            FSM.item('@literal', /^(.)/)
          ],
          token(match, content) {
            return {
              Type: 'Comment',
              Content: content.map(tok => tok.Content).join('')
            }
          }
        },
        FSM.item('PedalPress', /^&/),
        FSM.item('PedalRelease', /^\*/),
        FSM.item('Tie', /^\^/),
        FSM.item('Coda', /^\+/),
        FSM.item('Coda', /^Coda/),
        FSM.item('Coda', /^ToCoda/),
        FSM.item('Segno', /^s/),
        FSM.item('Segno', /^Segno/),
        FSM.item('DaCapo', /^DC/),
        FSM.item('DaSegno', /^DS/),
        FSM.item('Fine', /^Fine/),
        FSM.item('Space', /^(\s+)/)
      ],

      // Plain Function Arguments
      argument: [
        {
          patt: /^\)/,
          pop: true
        },
        {
          patt: /^, */
        },
        {
          patt: /^\[/,
          push: FSM.next('argument', /^\]/),
          token(match, content) {
            return {
              Type: 'Array',
              Content: content
            };
          }
        },
        {
          patt: /^"(([^\{\}\(\)\[\]\"\,]|\\.)*)"/,
          token(match) {
            return {
              Type: 'String',
              Content: match[1].replace(/\\(?=.)/, '')
            }
          }
        },
        FSM.item('Expression', /^([+\-]?\d+([./]\d+)?|Log2\(\d+\)([+\-]\d+)?)/),
        FSM.include('prototype')
      ],

      // Volta Numbers
      volta: [
        {
          patt: /(\d+)~(\d+)/,
          token(match) {
            const result = [];
            for (let i = parseInt(match[1]); i <= parseInt(match[2]); i++) {
              result.push(i);
            }
            return result;
          }
        },
        {
          patt: /\d+/,
          token: match => parseInt(match[0])
        },
        {
          patt: /^, */
        },
        {
          patt: /^\. */
        }
      ]

    });

    this.Contexts.alias = aliases.map(alias => new Alias(alias).build(dict));
  }
}

module.exports = TrackSyntax;


