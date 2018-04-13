const FSM = require('./Context');

const SpecialCharacters = [
  '(', ')', '[', ']', '{', '}',
  '?', '.', '+', '-', '*',
  '\\', '^', '$'
];

const AliasContexts = {
  default: [
    {
      patt: /^\$\{(\d+):/,
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
  ],
  class: [
    FSM.item('sub', /^sub(?:track)?/),
    FSM.item('exp', /^exp(?:ression)?/),
    FSM.item('int', /^int(?:eger)?/),
    FSM.item('sig', /^sig(?:ned)?/),
    FSM.item('uns', /^uns(?:igned)?/),
    FSM.item('not', /^notes/),
    FSM.item('nam', /^name/),
    FSM.item('mac', /^macro/),
    FSM.item('str', /^str(?:ing)?/),
    FSM.item('@und', /^[^\}]/)
  ]
}

class AliasSyntax extends FSM {
  
  constructor(declaration) {
    super(AliasContexts);
    const match = declaration.match(AliasSyntax.Pattern);
    this.Source = match[2].trim();
    this.Prec = match[1];
  }

  analyze() {
    const token = super.tokenize(this.Source, 'default', false);
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
        this.Syntax.shift();
      } else if (!this.Syntax[0].Class === 'mac') {
        this.Warnings.push({ Err: 'NotSubtrackL' });
      }
    }
    if (this.Syntax[this.Syntax.length - 1].Type === '@arg') {
      if (this.Syntax[this.Syntax.length - 1].Class === 'sub') {
        this.RightId = this.Syntax[this.Syntax.length - 1].Id;
        this.Syntax.pop();
      } else if (!this.Syntax[this.Syntax.length - 1].Class === 'mac') {
        this.Warnings.push({ Err: 'NotSubtrackR' });
      }
    }

    if (this.Prec === 0 || this.Prec >= FSM.MaxPrec) {
      this.Warnings.push({ 
        Err: 'PrecedenceRange',
        Prec: this.Prec
      });
      this.Prec = undefined;
    }

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
    delete this.Contexts;
    return this.Warnings.length === 0;
  }
}

class Alias {
  constructor(proto) {
    Object.assign(this, proto);
  }

  build(dict) {
    let pattern = '^';
    const index = [];
    const _this_ = this;

    this.Syntax.forEach(tok => {
      if (tok.Type === '@lit') {
        if (SpecialCharacters.includes(tok.Content)) {
          pattern += '\\';
        }
        pattern += tok.Content;
      } else {
        pattern += dict[tok.Class].patt;
        index.push(tok);
      }
    });

    return {
      patt: new RegExp(pattern),
      token(match) {
        const args = [];
        index.forEach((tok, index) => {
          let result = match[index + 1];
          if (dict[tok.Class].epilog) {
            result = dict[tok.Class].epilog(result);
          }
          args[tok.Id] = {
            Type: dict[tok.Class].meta,
            Content: result
          };
        });
        return {
          Type: '@alias',
          Args: args,
          VoidQ: _this_.VoidQ,
          Name: _this_.Name,
          LID: _this_.LeftId,
          RID: _this_.RightId,
          Prec: _this_.Prec,
          Order: _this_.Order
        };
      }
    };
  }
}

AliasSyntax.Pattern = /^(?: *prec(?:edence)?: *(\d+);)? *alias:(.+)$/i;

module.exports = {Alias, AliasSyntax};
