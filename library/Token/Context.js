const SubtrackTypes = [
  'Subtrack',
  'Macrotrack',
  'Note'
];

class FSM {

  constructor(source) {
    this.Contexts = source;
  }

  static include(name) {
    return name;
  }

  static item(name, regexp) {
    if (typeof regexp === 'string') {
      regexp = new RegExp('^(' + regexp + ')');
    }
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

  static next(name, ...event) {
    const result = event.map(regex => {
      return {
        patt: regex,
        pop: true
      };
    });
    result.push(name);
    return result;
  }

  // syntax:
  //   patt: regex
  //   push: sub-state
  //   pop: true
  //   token: callback

  tokenize(string, state, epi = true) {
    let valid = true, index = 0;
    let result = [], warnings = [];
    const syntax = this.getContext(state);

    while (string.length > 0) {
      let i, pop = false;
      for (i = 0; i < syntax.length; i++) {
        const stx = syntax[i];
        const match = string.match(stx.patt);
        if (match) {
          let content = [];
          const position = index;
          index += match[0].length;
          string = string.slice(match[0].length);
          if (stx.push) {
            const subtoken = this.tokenize(string, stx.push, epi);
            warnings.push(...subtoken.Warnings.map(msg => {
              return {
                Err: msg.Err,
                Src: msg.Src,
                Pos: msg.Pos + index
              };
            }));
            index += subtoken.Index;
            string = string.slice(subtoken.Index);
            content = subtoken.Content;
          }
          if (stx.pop) pop = true;
          if (stx.token) {
            const tok = stx.token(match, content);
            if (stx.locate === true || stx.locate === undefined) {
              Object.assign(tok, {Pos: position});
            }
            result.push(tok);
          }
          break;
        }
      }
      if (pop) break;
      if (i === syntax.length) {
        if (valid) {
          valid = false;
          warnings.push({
            Err: 'Undefined',
            Src: '',
            Pos: index
          });
        }
        warnings[warnings.length - 1].Src += string.charAt(0);
        string = string.slice(1);
        index += 1;
      } else {
        valid = true;
      }
    }

    if (epi) result = FSM.arrange(result);
    return {
      Index: index,
      Content: result,
      Warnings: warnings
    };
  }

  getContext(state) {
    let result;
    if (typeof state === 'string') {
      result = this.Contexts[state];
    } else {
      result = state;
    }
    let i = result.length;
    while (i--) {
      if (typeof result[i] === 'string') {
        result.splice(i, 1, ...this.getContext(result[i]));
      }
    }
    return result;
  }

  static isSubtrack(token) {
    if (SubtrackTypes.includes(token.Type)) {
      return true;
    } else if (token.Type === 'Function' && !token.VoidQ) {
      return true;
    } else {
      return false;
    }
  }

  static arrange(content) {
    let prior = FSM.findPrior(content);
    while (prior.Prec < FSM.MaxPrec) {
      let left = prior.Id, right = prior.Id;
      if (prior.LID != undefined) {
        left = prior.Id - 1;
        while (left >= 0 && !FSM.isSubtrack(content[left])) {
          left -= 1;
        }
        if (left < 0) {
          throw new Error();
        } else {
          prior.Args[prior.LID] = {
            Type: 'Subtrack',
            Content: content.slice(left, prior.Id)
          };
        }
      }
      if (prior.RID != undefined) {
        right = prior.Id + 1;
        while (right < content.length && !FSM.isSubtrack(content[right])) {
          right += 1;
        }
        if (right === content.length) {
          throw new Error();
        } else {
          prior.Args[prior.RID] = {
            Type: 'Subtrack',
            Content: content.slice(prior.Id + 1, right + 1)
          };
        }
      }
      content.splice(left, right - left + 1, {
        Type: 'Function',
        Name: prior.Name,
        Args: prior.Args,
        Pos: prior.Pos,
        Alias: prior.Order,
        VoidQ: prior.VoidQ
      });
      prior = FSM.findPrior(content);
    }
    return content;
  }

  static findPrior(content) {
    let prior = { Prec: FSM.MaxPrec };
    content.forEach((tok, index) => {
      if (tok.Type === '@alias' && tok.Prec < prior.Prec) {
        prior = Object.assign(tok, {Id: index});
      }
    });
    return prior;
  }
}

FSM.MaxPrec = 10000;

module.exports = FSM;



