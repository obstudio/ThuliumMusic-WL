const FSM = require('./Context');

const AliasContexts = {
  default: {
    syntax: [
      {
        patt: /^\$\{(\d+):/,
        push: FSM.next('class', /^\}/),
        token(match, content) {
          if (content.length === 1) {
            return {
              Type: content[0].Type,
              Id: match[1]
            };
          } else {
            return {
              Type: '@invarg'
            };
          }
        }
      },
      FSM.item('@literal', /^(.)/)
    ]
  },
  class: {
    syntax: [
      FSM.item('#expression', /^exp(?:ression)?/),
      FSM.item('#integer', /^int(?:eger)?/),
      FSM.item('#signed', /^sig(?:ned)?/),
      FSM.item('#unsigned', /^uns(?:igned)?/),
      FSM.item('#subtrack', /^sub(?:track)?/),
      FSM.item('#notes', /^notes/),
      FSM.item('#name', /^name/),
      FSM.item('#string', /^str(?:ing)?/),
      FSM.item('@undefined', /^[^\}]/)
    ]
  }
}

const InvalidLiterals = ['{', '}', '[', ']', '"'];

class Alias extends FSM {
  
  constructor(declaration) {
    super(AliasContexts);
    const match = declaration.match(Alias.Pattern);
    this.Source = match[2].trim();
    this.Syntax = super.tokenize(this.Source, 'default').Content;
    this.Prec = match[1];
  }

  analyze() {
    this.Warnings = [];

    // Check Balance & Class & Literate
    let balance = 0;
    this.Syntax.forEach((tok, index) => {
      if (tok.Type === '@invarg') {
        this.Warnings.push({
          Err: 'InvalidArgument',
          Src: this.Source.slice(this.Syntax[index - 1].Pos, this.Syntax[index].Pos),
          Pos: this.Syntax[index - 1].Pos + 1
        });
      }
      if (tok.Type === '@literal') {
        if (InvalidLiterals.includes(tok.Content)) {
          this.Warnings.push({
            Err: 'InvalidLiteral',
            Src: tok.Content,
            Pos: tok.Pos
          });
        } else if (tok.Content === '(') {
          balance += 1;
        } else if (tok.Content === ')') {
          balance -= 1;
          if (balance < 0) {
            this.Warnings.push({
              Err: 'UnblancedBracket',
              Pos: tok.Pos
            })
          }
        }
      }
    });
    if (balance < 0) {
      this.Warnings.push({
        Err: 'UnblancedBracket',
        Pos: this.Source.lastIndexOf('(')
      });
    }
    if (this.Warnings.length > 0) return false;
    return true;
  }

}

Alias.Pattern = /^(?: *prec(?:edence)?: *(\d+);)? *alias:(.+)$/i;

module.exports = Alias;

console.log(new Alias(' alias: (${1:int}~)${2:sub}').analyze());
