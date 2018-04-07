const FSM = require('./Context');

const SpecialCharacters = [
  '(', ')', '[', ']', '{', '}',
  '?', '.', '+', '-', '*',
  '\\', '^', '$'
];

const AliasContexts = {
  default: {
    syntax: [
      {
        patt: /^\$\{([1-9]\d*):/,
        push: FSM.next('class', /^\}/),
        token(match, content) {
          if (content.length === 1 && content[0].Type != '@und') {
            return {
              Type: '@arg',
              Class: content[0].Type,
              Id: parseInt(match[1])
            };
          } else {
            return {
              Type: '@inv'
            };
          }
        }
      },
      FSM.item('@lit', /^([^\{\}\[\]"])/)
    ]
  },
  class: {
    syntax: [
      FSM.item('sub', /^sub(?:track)?/),
      FSM.item('exp', /^exp(?:ression)?/),
      FSM.item('int', /^int(?:eger)?/),
      FSM.item('sig', /^sig(?:ned)?/),
      FSM.item('uns', /^uns(?:igned)?/),
      FSM.item('not', /^notes/),
      FSM.item('nam', /^name/),
      FSM.item('str', /^str(?:ing)?/),
      FSM.item('und', /^[^\}]/)
    ]
  }
}

class Alias extends FSM {
  
  constructor(declaration) {
    super(AliasContexts);
    const match = declaration.match(Alias.Pattern);
    this.Source = match[2].trim();
    this.Prec = match[1];
  }

  analyze() {
    const token = super.tokenize(this.Source, 'default');
    token.Warnings.forEach(err => err.Err = 'InvalidLiteral');
    this.Syntax = token.Content;
    this.Warnings = token.Warnings;

    // Check Balance & Class & Literate
    let balance = 0, dict = [];
    this.Syntax.forEach((tok, index) => {

      if (tok.Type === '@inv') {
        this.Warnings.push({
          Err: 'InvalidArgument',
          Src: this.Source.slice(tok.Pos, this.Syntax[index + 1].Pos),
          Pos: tok.Pos
        });
      }

      if (tok.Type === '@arg') {
        if (tok.Class === 'sub' && index > 0 && index < this.Syntax.length - 1) {
          this.Warnings.push({
            Err: 'SubtrackInside',
            Pos: tok.Pos
          });
        }
        if (dict[tok.Id]) {
          this.Warnings.push({
            Err: 'DuplicateId',
            Id: tok.Id,
            Pos: tok.Pos + 2
          });
        } else {
          dict[tok.Id] = tok.Pos;
        }
      }

      if (tok.Type === '@lit') {
        if (tok.Content === '(') balance += 1;
        if (tok.Content === ')') {
          balance -= 1;
          if (balance < 0) {
            this.Warnings.push({
              Err: 'UnblancedBracket',
              Pos: tok.Pos
            });
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
    if (this.Syntax.every(tok => tok.Type === '@arg')) {
      this.Warnings.push({ Err: 'AllArgument' });
    }
    if (this.Warnings.length > 0) return false;

    // Check Argument Class
    if (this.Syntax[0].Type === '@arg') {
      if (this.Syntax[0].Class === 'sub') {
        this.LeftId = this.Syntax[0].Id;
      } else {
        this.Warnings.push({ Err: 'NotSubtrackL' });
      }
    }
    this.Syntax.unshift();
    if (this.Syntax[this.Syntax.length - 1].Type === '@arg') {
      if (this.Syntax[this.Syntax.length - 1].Class === 'sub') {
        this.RightId = this.Syntax[this.Syntax.length - 1].Id;
      } else {
        this.Warnings.push({ Err: 'NotSubtrackR' });
      }
    }
    this.Syntax.pop();

    if (!this.Prec) {
      if (this.LeftId && this.RightId) {
        this.Prec = 400;
      } else if (this.RightId) {
        this.Prec = 300;
      } else if (this.LeftId) {
        this.Prec = 200;
      } else {
        this.Prec = 100;
      }
    }

    return this.Warnings.length === 0;
  }

  build(pattdict) {
    let pattern = '';
    const index = [];
    const _this = this;

    this.Syntax.forEach(tok => {
      if (tok.Type === '@lit') {
        if (SpecialCharacters.includes(tok.Content)) {
          pattern += '\\';
        }
        pattern += tok.Content;
      } else {
        pattern += pattdict[tok.Class];
        index.push(tok.Id);
      }
    });

    return {
      patt: pattern,
      token(match) {
        const args = [];
        index.forEach((id, index) => args[id] = match[index]);
        return {
          Type: '@alias',
          Args: args,
          Name: _this.Name,
          LID: _this.LeftId,
          RID: _this.RightId,
          Prec: _this.Prec
        };
      }
    }
  }

}

Alias.Pattern = /^(?: *prec(?:edence)?: *(\d+);)? *alias:(.+)$/i;

module.exports = Alias;
