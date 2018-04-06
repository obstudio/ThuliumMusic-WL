
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

class TrackSyntax {
  constructor(funcStx, noteStx) {
    this.funcStx = funcStx;
    this.contexts = {

      // Subtrack & Macrotrack & PlainFunction
      Prototype: {
        syntax: [
          {
            patt: /^\{(?:(\d+)\*)?/,
            push: 'Default',
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
            push: 'Argument',
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

      // Track Contents
      Default: {
        include: ['Prototype'],
        syntax: [
          {
            patt: /^\}/,
            pop: true
          },
          {
            patt: /^\(([a-zA-Z][a-zA-Z\d]*):/,
            push: 'Argument',
            token(match, content) {
              return {
                Type: 'Function',
                Name: match[1],
                Alias: 0,
                Args: content
              };
            }
          },
          this.item('Note', NotePattern(noteStx)),
          this.item('RepeatEndBegin', /^:\|\|:/),
          this.item('RepeatBegin', /^\|\|:/),
          this.item('RepeatEnd', /^:\|\|/),
          this.item('@Local', /^!/),
          {
            patt: /^\[(?=(\d+(~\d+)\. *)+\])/,
            push: this.next('Volta', /\]/),
            token(match, content) {
              return {
                Type: 'Volta',
                Order: [].concat(...content)
              };
            }
          },
          {
            patt: /^\\(?=(\d+(~\d+))(, *(\d+(~\d+)))*:)/,
            push: this.next('Volta', /:/),
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
          this.item('PedalPress', /^&/),
          this.item('PedalRelease', /^\*/),
          this.item('Coda', /^\+/),
          this.item('Coda', /^Coda/),
          this.item('Coda', /^ToCoda/),
          this.item('Segno', /^s/),
          this.item('Segno', /^Segno/),
          this.item('DaCapo', /^DC/),
          this.item('DaSegno', /^DS/),
          this.item('Fine', /^Fine/)
        ]
      },

      // Plain Function Arguments
      Argument: {
        include: ['Prototype'],
        syntax: [
          {
            patt: /^\)/,
            pop: true
          },
          {
            patt: /^\[/,
            push: this.next('Argument', /^\]/),
            token(match, content) {
              return {
                Type: 'Array',
                Content: content
              };
            }
          },
          this.item('Number', /^([+\-]?\d+([./]\d+)?|Log2\(\d+\)([+\-]\d+)?)/),
          this.item('String', /^"(([^"]|\\.)*)"/),
          {
            patt: /^, */
          }
        ]
      },

      // Volta Numbers
      Volta: {
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

    };
  }

  item(name, regexp) {
    return {
      patt: regexp,
      token(match) {
        const result = { Type: name };
        if (match[1]) {
          Object.assign(result, { Content: match[1] });
        }
        return result;
      }
    };
  }

  next(name, ...event) {
    return {
      include: [name],
      syntax: event.map(regex => {
        return {
          patt: regex,
          pop: true
        };
      })
    };
  }

  // include: state names
  // syntax:
  //   patt: regex
  //   push: sub-state
  //   pop: true
  //   token: callback

  tokenize(string, state = 'Default') {
    let valid = true, index = 0;
    const result = [], warnings = [];
    const syntax = this.getContext(state);

    while (string.length > 0) {
      let i, pop = false;
      for (i = 0; i < syntax.length; i++) {
        const stx = syntax[i];
        const match = string.match(stx.patt);
        if (match) {
          let content = [];
          index += match[0].length;
          string = string.slice(match[0].length);
          if (stx.push) {
            const subtoken = this.tokenize(string, stx.push);
            warnings.push(...subtoken.Warnings.map(msg => {
              return {
                Err: msg.Err,
                Args: msg.Args,
                Pos: msg.Pos + index
              };
            }));
            index += subtoken.Index;
            string = string.slice(subtoken.Index);
            content = subtoken.Content;
          }
          if (stx.pop) pop = true;
          if (stx.token) result.push(Object.assign(stx.token(match, content), {Pos: index}));
          break;
        }
      }
      if (pop) break;
      if (i === syntax.length) {
        if (valid) {
          valid = false;
          warnings.push({
            Err: 'Undefined',
            Args: '',
            Pos: index
          });
        }
        warnings[warnings.length - 1].Args += string.charAt(0);
        string = string.slice(1);
        index += 1;
      } else {
        valid = true;
      }
    }

    return {
      Index: index,
      Content: result,
      Warnings: warnings
    };
  }

  getContext(state) {
    if (typeof state === 'string') state = this.contexts[state];
    if (!('syntax' in state)) throw new Error();
    const result = state.syntax;
    if (state.include) {
      state.include.forEach(state => {
        result.unshift(...this.contexts[state].syntax);
      });
    }
    return result;
  }

}

module.exports = TrackSyntax;



