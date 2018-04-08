class FSM {

  constructor(source) {
    this.Contexts = source;
  }

  static include(name) {
    return name;
  }

  static autopop() {
    return {
      patt: /^(?=.)/,
      pop: true
    };
  }

  static ahead(str) {
    return new RegExp('^(?=' + str + ')');
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

  tokenize(string, state) {
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
          const position = index;
          index += match[0].length;
          string = string.slice(match[0].length);
          if (stx.push) {
            const subtoken = this.tokenize(string, stx.push);
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
            result.push(Object.assign(stx.token(match, content), {Pos: position}));
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

}

module.exports = FSM;



