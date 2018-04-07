const acorn = require('acorn');

const int = '([+\\-]?\\d+)';
const item = `(\\[${int}?(:${int}?)?\\])?${int}?`;
const exp = `(${item}(, *${item})*)`;
const def = `([a-zA-Z])\\t+(?:([^\\t]+)\\t+)?`;
const ChordItem = new RegExp(`${item}`);
const ChordPatt = new RegExp(`^${def}${exp}$`);

const AliasPatt = /^(?: *prec(?:edence)?:(\d+);) *alias:(.+)$/i;

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
        return false
      }
      if (funcTypes.includes(node.type)) {
        return true
      }
      if ('body' in node) {
        if (node.body instanceof Array) {
          return node.body.every(walk)
        } else {
          return walk(node)
        }
      }
      switch (node.type) {
        case 'IfStatement':
          return walk(node.consequent) && (!node.alternate || walk(node.alternate))
        case 'SwitchStatement':
          return node.cases.every((sub) => sub.consequent.every(walk))
        case 'ThrowStatement':
          throw new Error('With throw')
        case 'TryStatement':
          return walk(node.block) 
              && (!node.handler || walk(node.handler.body)) 
              && (!node.finalizer || walk(node.finalizer))
        default:
          return true
      }
    }
    return walk(funcAST.body)
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
      Data: data,
      Warnings: warnings
    };
  }

  static FunctionTokenize(code) {
    const data = [], errors = [], warnings = [];
    let result, alias = [];
    try {
      result = acorn.parse(code, {
        ecmaVersion: 8,
        onComment(isBlock, text, start, end) {
          const result = AliasPatt.exec(text);
          if (!isBlock && result) {
            let prec = result[1];
            if (prec) prec = parseInt(prec.trim());
            alias.push({
              prec: prec,
              stx: result[2].trim(),
              start: start,
              end: end
            });
          }
        }
      });
      result.body.forEach((tok) => {
        if (tok.type === 'FunctionDeclaration') {
          let i = alias.length;
          while (i--) {
            if (tok.body.start < alias[i].start && tok.body.end > alias[i].end) {
              let prec = alias[i].prec;
              const voidQ = LibTokenizer.isVoid(tok);
              const syntax = LibTokenizer.parseAlias(alias[i]);
              if (syntax.warnings.length > 0) {
                warnings.push({
                  Type: 'AliasError',
                  Src: alias[i],
                  Msg: syntax.warnings
                });
                continue;
              }
              if (prec <= 0 || prec > 10000) {
                warnings.push({
                  Type: 'InvalidPrec',
                  Prec: prec
                });
                continue;
              }
              if (!prec) {
                if (syntax.Syntax.ArgCount === 0) {
                  prec = 100;
                } else if (syntax.Syntax.ArgCount === 1) {
                  prec = 200;
                } else {
                  prec = 300;
                }
              }
              data.push({
                Name: tok.id.name,
                Syntax: alias[i].stx,
                VoidQ: voidQ,
                Prec: prec
              });
              alias.splice(i, 1);
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
      Data: data,
      Errors: errors,
      Warnings: warnings
    };
  }

  parseAlias(alias) {
    let leftArg = rightArg = false;
    let argCount = 0;
    const warnings = [];
    return {
      Syntax: {
        Left: leftArg,
        Right: rightArg
      },
      Warnings: warnings
    };
  }

}

module.exports = LibTokenizer;
