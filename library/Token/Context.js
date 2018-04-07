class FSM {

  constructor(source) {
    this.Contexts = source;
  }

  static include(name) {
    return name;
  }

  static item(name, regexp) {
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
    return [name,
      ...event.map(regex => {
        return {
          patt: regex,
          pop: true
        };
      })
    ];
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
    if (typeof state === 'string') state = this.Contexts[state];
    let i = state.length;
    while (i--) {
      if (typeof state[i] === 'string') {
        state.splice(i, 1, ...this.Contexts[state[i]]);
      }
    }
    return state;
  }

}

module.exports = FSM;



