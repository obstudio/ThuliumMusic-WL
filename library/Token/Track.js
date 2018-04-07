const FSM = require('./Context');

const GeneralNoteSyntax = {
  Degree: ['1', '2', '3', '4', '5', '6', '7', '0', 'x', '%'],
  PitOp: ["'", ',', '#', 'b'],
  Chord: [],
  VolOp: [':', '>'],
  DurOp: ['-', '=', '_', '.'],
  Epilog: ['`']
};

function SyntaxJoin(...arr) {
  return arr.map(arr => {
    if (arr.length === 0) {
      return ''
    } else {
      return '[' + arr.join('') + ']'
    }
  }).join('');
}

function NotePattern(stx) {
  const InnerOp = SyntaxJoin(stx.PitOp, stx.Chord, stx.VolOp);
  const OuterOp = SyntaxJoin(stx.DurOp, stx.Epilog);
  const patt1 = stx.Degree + InnerOp + OuterOp;
  const patt2 = '\\[(' + stx.Degree + InnerOp + ')+\\]' + OuterOp;
  return new RegExp('^(' + patt1 + '|' + patt2 + ')');
}

class TrackSyntax extends FSM {
  constructor(funcStx, noteStx) {
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
        include: ['prototype'],
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
          FSM.item('Note', NotePattern(noteStx)),
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
            patt: /^\[/,
            push: FSM.next('argument', /^\]/),
            token(match, content) {
              return {
                Type: 'Array',
                Content: content
              };
            }
          },
          FSM.item('Number', /^([+\-]?\d+([./]\d+)?|Log2\(\d+\)([+\-]\d+)?)/),
          FSM.item('String', /^"(([^"]|\\.)*)"/),
          {
            patt: /^, */
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
  }

}

module.exports = TrackSyntax;

console.log(new TrackSyntax({},GeneralNoteSyntax).tokenize('{dd}','default'));

