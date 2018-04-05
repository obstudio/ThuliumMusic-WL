class NoteSyntax {
  constructor(stx) {
    const InnerOp = this.SyntaxJoin(stx.PitOp, stx.Chord, stx.VolOp);
    const OuterOp = this.SyntaxJoin(stx.DurOp, stx.Epilog);
    const patt1 = stx.Degree + InnerOp + OuterOp;
    const patt2 = '\\[(' + stx.Degree + InnerOp + ')+\\]' + OuterOp;
    this.patt = new RegExp('^(' + patt1 + '|' + patt2 + ')');
  }
  syntaxJoin() {
    return arguments.map(str => {
      if (str === '') {
        return ''
      } else {
        return '[' + str + ']'
      }
    }).join('');
  }
}

class Syntax {
  constructor(funcStx, noteStx) {
    const NotePatt = new NoteSyntax(noteStx).patt;
    
    this.contexts = {

      // Subtrack & Macrotrack & PlainFunction
      Prototype: {
        syntax: [
          {
            patt: /^\{(?:(\d+)\*)?/,
            push: this.Next('Default', /^\}/),
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
            push: this.Next('Argument', /^\)/),
            token(match, content) {
              return {
                Type: 'Function',
                Name: match[1],
                Alias: -1,
                Args: content
              };
            }
          },
          this.Item('Macrotrack', /^@([a-zA-Z]\w*)/)
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
            push: this.Next('Argument', /^\)/),
            token(match, content) {
              return {
                Type: 'Function',
                Name: match[1],
                Alias: 0,
                Args: content
              };
            }
          },
          this.Item('Comment', /<\*(([^*]|\*[^>])*)\*>/),
          this.Item('RepeatEndBegin', /:\|\|:/),
          this.Item('RepeatBegin', /\|\|:/),
          this.Item('RepeatEnd', /:\|\|/),
          this.Item('Note', NotePatt),
          this.Item('@Local', /!/),
          {
            patt: /^\[(?=(\d+(~\d+)\. *)+\])/,
            push: this.Next('Volta', /\]/),
            token(match, content) {
              return {
                Type: 'Volta',
                Order: [].concat(...content)
              };
            }
          },
          {
            patt: /^\\(?=(\d+(~\d+))(, *(\d+(~\d+)))*:)/,
            push: this.Next('Volta', /:/),
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
            patt: /^\/|\||\\/,
            token(match) {
              return {
                Type: 'BarLine',
                Skip: match[0] === '\\',
                Overlay: match[0] === '/',
                Order: [0]
              };
            }
          },
          this.Item('PedalPress', /&/),
          this.Item('PedalRelease', /\*/),
          this.Item('Coda', /\+/),
          this.Item('Coda', /Coda/),
          this.Item('Coda', /ToCoda/),
          this.Item('Segno', /s/),
          this.Item('Segno', /Segno/),
          this.Item('DaCapo', /DC/),
          this.Item('DaSegno', /DS/),
          this.Item('Fine', /Fine/)
        ]
      },

      // Plain Function Arguments
      Argument: {
        include: ['Prototype'],
        syntax: [
          {
            patt: /^\[/,
            push: this.Next('Argument', /^\]/),
            token(match, content) {
              return {
                Type: 'Array',
                Content: content
              };
            }
          },
          this.Item('Number', /^([+\-]?\d+([./]\d+)|Log2\(\d+\)([+\-]\d+)?)/),
          this.Item('String', /^"(([^"]|\\.)*)"/),
          {
            patt: /^,\s*/
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
            token: (match) => parseInt(match[0])
          },
          {
            patt: /, */
          },
          {
            patt: /^\. */
          }
        ]
      }

    };
    this.funcStx = funcStx;
  }

  Item(name, regexp) {
    return {
      patt: regexp,
      token(match) {
        const result = { Type: name };
        if (match[1] != undefined) {
          Object.assign(result, { Content: match[1] });
        }
        return result;
      }
    };
  }

  Next(name, event) {
    return {
      include: [name],
      syntax: [
        {
          patt: event,
          pop: true
        }
      ]
    };
  }

  // include: state names
  // syntax:
  //   patt: regex
  //   push: sub-state
  //   pop: true
  //   token: callback

  tokenize(string) {

  }

}

module.exports = Syntax;

