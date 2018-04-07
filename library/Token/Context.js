class FSM {

  constructor(source) {
    this.Contexts = source;
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
          if (stx.token) {
            result.push(Object.assign(stx.token(match, content), {Pos: index}));
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
    if (typeof state === 'string') state = this.Contexts[state];
    if (!('syntax' in state)) throw new Error();
    const result = state.syntax;
    if (state.include) {
      state.include.forEach(state => {
        result.unshift(...this.Contexts[state].syntax);
      });
    }
    return result;
  }

}

module.exports = FSM;



