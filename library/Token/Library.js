const acorn = require('acorn');
const {AliasSyntax} = require('./Alias');

const int = '([+\\-]?\\d+)';
const item = `(\\[${int}?(:${int}?)?\\])?${int}?`;
const exp = `(${item}(, *${item})*)`;
const def = `([a-zA-Z])\\t+(?:([^\\t]+)\\t+)?`;
const ChordItem = new RegExp(`${item}`);
const ChordPatt = new RegExp(`^${def}${exp}$`);

const funcTypes = [
  'FunctionExpression', 
  'ArrowFunctionExpression', 
  'FunctionDeclaration', 
  'ClassDeclaration', 
  'ClassExpression'
]

class LibTokenizer {
  /**
   * 判断函数是否无返回值
   * @param {ESTree.FunctionDeclaration} funcAST 函数声明节点
   * @returns {boolean} 当至少有一个支路上包含return语句时返回false，否则返回true
   * @throws 如果ast包含throw语句，isVoid将丢出一个错误
   */
  static isVoid(funcAST) {
    function walk(node) {
      if (node.type === 'ReturnStatement') {
        return !node.argument;
      }
      if (funcTypes.includes(node.type)) {
        return true;
      }
      if ('body' in node) {
        if (node.body instanceof Array) {
          return node.body.every(walk);
        } else {
          return walk(node.body);
        }
      }
      switch (node.type) {
        case 'IfStatement':
          return walk(node.consequent) && (!node.alternate || walk(node.alternate));
        case 'SwitchStatement':
          return node.cases.every((sub) => sub.consequent.every(walk));
        case 'ThrowStatement':
          throw new Error('With throw');
        case 'TryStatement':
          return walk(node.block) 
              && (!node.handler || walk(node.handler.body)) 
              && (!node.finalizer || walk(node.finalizer));
        default:
          return true;
      }
    }
    return walk(funcAST.body);
  }

  static ChordTokenize(lines) {
    const data = [], warnings = [];
    lines.forEach(line => {
      const match = line.match(ChordPatt);
      if (match) {
        const notation = match[1];
        const comment = match[2];
        const pitches = match[3].split(/, */).map(item => {
          const data = item.match(ChordItem);
          return [
            data[2] ? parseInt(data[2]) : 0,
            data[4] ? parseInt(data[4]) :
              data[3] ? -1 :
                data[2] ? parseInt(data[2]) :
                  data[1] ? -1 : 0,
            data[5] ? parseInt(data[5]) : 0
          ];
        });
        data.push({
          Notation: notation,
          Comment: comment,
          Pitches: pitches
        });
      } else {
        if (!line.match(/^\s*$/)) {
          warnings.push({
            Err: 'InvChordDecl',
            Data: line
          });
        }
      }
    });
    return {
      Chord: data,
      Errors: [],
      Warnings: warnings
    };
  }

  static FunctionTokenize(code) {
    const aliases = [], errors = [], warnings = [];
    const dict = [], syntax = [];
    try {
      const result = acorn.parse(code, {
        ecmaVersion: 8,
        onComment(isBlock, text, start, end) {
          const result = AliasSyntax.Pattern.exec(text);
          if (!isBlock && result) {
            const alias = new AliasSyntax(text);
            if (alias.analyze()) {
              syntax.push({start, end, alias});
            } else {
              warnings.push(...alias.Warnings);
            }
          }
        }
      });
      result.body.forEach((tok) => {
        if (tok.type === 'FunctionDeclaration') {
          const name = tok.id.name;
          const voidQ = LibTokenizer.isVoid(tok);
          dict.push({
            Name: name,
            VoidQ: voidQ
          });
          let order = 0;
          for (let i = 0; i < syntax.length; i++) {
            if (tok.body.start < syntax[i].start && tok.body.end > syntax[i].end) {
              order += 1;
              aliases.push(Object.assign(syntax[i].alias, {
                Name: name,
                Order: order,
                VoidQ: voidQ
              }));
            }
          }
        } else {
          errors.push({
            Err: 'NotFuncDecl',
            Type: tok.type,
            Start: tok.start,
            End: tok.end
          });
        }
      });
    } catch (err) {
      errors.push({
        Err: 'SyntaxError',
        Info: err
      });
    }
        
    return {
      Alias: aliases,
      Dict: dict,
      Code: code,
      Errors: errors,
      Warnings: warnings
    };
  }

}

module.exports = LibTokenizer;
