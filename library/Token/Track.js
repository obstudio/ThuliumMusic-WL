const FSM = require('./Context');

class NoteSyntax {
  constructor(degrees, chords) {
    this.Degree = NoteSyntax.ArrayToRegex(degrees, false);
    this.Chord = NoteSyntax.ArrayToRegex(chords, true);
    this.PitOp = "[#b',]*";
    this.DurOp = '[._=-]*'
    this.VolOp = '[>:]*';
    this.Epilog = '[`]*';
  }

  pattern() {
    const inner = this.Degree + this.PitOp + this.Chord + this.VolOp;
    const outer = this.DurOp + this.Epilog;
    return '((?:\\[(?:' + inner + ')+\\]|'+ inner + ')' + outer + ')';
  }

  static ArrayToRegex(array, multi = true) {
    let charset = '', quantifier = '';
    if (array.length > 0) charset = '[' + array.join('') + ']';
    if (multi && array.length > 0) quantifier = '*';
    return charset + quantifier;
  }
}

class TrackSyntax extends FSM {
  constructor(aliases, degrees, chords) {
    const dict = {
      uns: '(\\d+)',
      sig: '([+\\-]\\d+)',
      int: '([+\\-]?\\d+)',
      exp: '([+\\-]?\\d+(?:[./]\\d+)?|Log2\\(\\d+\\)(?:[+\\-]\\d+)?)',
      str: '((?:[^\\{\\}\\(\\)\\[\\]\\"\\,]|\\\\.)*)',
      nam: '([a-zA-Z][a-zA-Z\\d]*)',
      not: new NoteSyntax(degrees, chords).pattern()
    };
    super({

      // Subtrack & Macrotrack & PlainFunction
      prototype: {
        syntax: [
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
            patt: /^([a-zA-Z][a-zA-Z\d]*)\(/,
            push: 'argument',
            token(match, content) {
              return {
                Type: 'Function',
                Name: match[1],
                Alias: -1,
                Args: content
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
        ]
      },

      init: {
        syntax: [
          {
            patt: /^</,
            push: {
              syntax: [
                {
                  patt: /^>/,
                  pop: true
                },
                {
                  patt: /^(:)(?:([a-zA-Z][a-zA-Z\d]*):)/,
                  token(match) {
                    return {
                      Type: '@name',
                      name: match[2],
                      macro: Boolean(match[1])
                    };
                  }
                },
                {
                  patt: /[a-zA-Z][a-zA-Z\d]*/,
                  push: FSM.next('default', /^, */, /^(?=>)/),
                  token(match, content) {
                    return {
                      Type: '@inst',
                      name: match[0],
                      spec: content
                    };
                  }
                },
                {
                  patt: /, */
                }
              ]
            },
            token(match, content) {
              return {
                Type: '@meta',
                Name: '',
                Instruments: []
              };
            }
          },
          'default'
        ]
      },

      // Track Contents
      default: {
        include: ['prototype', 'alias'],
        syntax: [
          {
            patt: /^\}/,
            pop: true
          },
          {
            patt: /^\(([a-zA-Z][a-zA-Z\d]*):/,
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
          FSM.item('Note', new RegExp('^' + dict.not)),
          FSM.item('RepeatEndBegin', /^:\|\|:/),
          FSM.item('RepeatBegin', /^\|\|:/),
          FSM.item('RepeatEnd', /^:\|\|/),
          FSM.item('LocalIndicator', /^!/),
          {
            patt: /^\[(?=(\d+(~\d+)\. *)+\])/,
            push: FSM.next('volta', /\]/),
            token(match, content) {
              return {
                Type: 'volta',
                Order: [].concat(...content)
              };
            }
          },
          {
            patt: /^\\(?=(\d+(~\d+))(, *(\d+(~\d+)))*:)/,
            push: FSM.next('volta', /:/),
            token(match, content) {
              return {
                Type: 'BarLine',
                Skip: false,
                Overlay: false,
                Order: [].concat(...content)
              };
            }
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
          FSM.item('PedalPress', /^&/),
          FSM.item('PedalRelease', /^\*/),
          FSM.item('Coda', /^\+/),
          FSM.item('Coda', /^Coda/),
          FSM.item('Coda', /^ToCoda/),
          FSM.item('Segno', /^s/),
          FSM.item('Segno', /^Segno/),
          FSM.item('DaCapo', /^DC/),
          FSM.item('DaSegno', /^DS/),
          FSM.item('Fine', /^Fine/)
        ]
      },

      // Plain Function Arguments
      argument: {
        include: ['prototype'],
        syntax: [
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
          FSM.item('Expression', new RegExp('^' + dict.exp)),
          {
            patt: new RegExp('^"' + dict.str + '"'),
            token(match) {
              return {
                Type: 'String',
                Content: match[1].replace(/\\(?=.)/, '')
              }
            }
          }
        ]
      },

      // Volta Numbers
      volta: {
        syntax: [
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
      }

    });

    this.Contexts.alias = {
      syntax: aliases.map(alias => alias.build())
    };
  }
}

module.exports = TrackSyntax;

console.log(new TrackSyntax([],
  ['1', '2', '3', '4', '5', '6', '7', '0', 'x', '%'],
[]).tokenize('foo(Log2(3),"2")','default').Content[0]);

