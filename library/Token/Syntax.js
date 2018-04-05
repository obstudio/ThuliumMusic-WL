const sDef = [
  {
    pat: [
      {
        Type: 'Sfunc',
        Content: [
          /^(\d+)-/
        ]
      },
      {
        Type: 'Subtrack'
      }
    ],
    transform(match) {
      return {
        Type: 'Function',
        Name: 'Tremolo1',
        Simplified: true,
        Argument: [
          {
            Type: 'Expression',
            Content: match[0].Content[0].Content.slice(0, -1)
          },
          match[1]
        ]
      }
    }
  },
  {
    pat: [
      {
        Type: 'Subtrack'
      },
      {
        Type: 'Sfunc',
        Content: [
          /^(\d+)=/
        ]
      },
      {
        Type: 'Subtrack'
      }
    ],
    transform(match) {
      return {
        Type: 'Function',
        Name: 'Tremolo2',
        Simplified: true,
        Argument: [
          {
            Type: 'Expression',
            Content: match[1].Content[0].Content.slice(0, -1)
          },
          match[0],
          match[2]
        ]
      }
    }
  },
  {
    pat: [
      {
        Type: 'Subtrack'
      },
      {
        Type: 'Undef',
        Content: '~'
      },
      {
        Type: 'Subtrack'
      }
    ],
    transform(match) {
      return {
        Type: 'Function',
        Name: 'Portamento',
        Simplified: true,
        Argument: [
          match[0],
          match[2]
        ]
      }
    }
  },
  {
    pat: [
      {
        Type: 'Undef',
        Content: '$'
      },
      {
        Type: 'Note'
      }
    ],
    transform(match) {
      return {
        Type: 'Function',
        Name: 'Arpeggio',
        Simplified: true,
        Argument: [
          {
            Type: 'Subtrack',
            Repeat: -1,
            Content: [match[1]]
          }
        ]
      }
    }
  },
  {
    pat: [
      {
        Type: 'Sfunc',
        Content: [
          /^\./
        ]
      },
      {
        Type: ['Note', 'Function']
      }
    ],
    transform(match) {
      return {
        Type: 'Function',
        Name: 'Fermata',
        Simplified: true,
        Argument: [
          {
            Type: 'Subtrack',
            Repeat: -1,
            Content: [match[1]]
          }
        ]
      }
    }
  },
  {
    pat: [
      {
        Type: 'Sfunc',
        Content: [
          /(\d+)~/
        ]
      },
      {
        Type: 'Subtrack'
      }
    ],
    transform(match) {
      return {
        Type: 'Function',
        Name: 'Tuplet',
        Simplified: true,
        Argument: [
          {
            Type: 'Expression',
            Content: match[0].Content[0].Content.slice(0, -1)
          },
          match[1]
        ]
      }
    }
  },
  {
    pat: [
      {
        Type: 'Sfunc',
        Content: [
          /^(([0-7x%])([',#b]*)([A-Zac-wyz]*)([',#b]*)([-_.=]*)(`*)([:>]*)|\[(([0-7x%][',#A-Za-wyz:>]*)+)\]([',#b]*)([-_.=]*)(`*)([:>]*))+\^/
        ]
      },
      {
        Type: 'Note'
      }
    ],
    transform(match, tok) {
      return {
        Type: 'Function',
        Name: 'GraceNote',
        Simplified: true,
        Argument: [
          {
            Type: 'Subtrack',
            Content: tok(match[0].Content[0].Content.slice(0, -1)),
            Repeat: -1
          },
          {
            Type: 'Subtrack',
            Content: [match[1]],
            Repeat: -1
          }
        ]
      }
    }
  },
  {
    pat: [
      {
        Type: 'Note'
      },
      {
        Type: 'Sfunc',
        Content: [
          /^\^(([0-7x%])([',#b]*)([A-Zac-wyz]*)([',#b]*)([-_.=]*)(`*)([:>]*)|\[(([0-7x%][',#A-Za-wyz:>]*)+)\]([',#b]*)([-_.=]*)(`*)([:>]*))+/
        ]
      }
    ],
    transform(match, tok) {
      return {
        Type: 'Function',
        Name: 'Appoggiatura',
        Simplified: true,
        Argument: [
          {
            Type: 'Subtrack',
            Content: [match[0]],
            Repeat: -1
          },
          {
            Type: 'Subtrack',
            Content: tok(match[1].Content[0].Content.slice(1)),
            Repeat: -1
          }
        ]
      }
    }
  }
]

const langDef = {
  Dynamic: [
    {
      regex: /^{(\d+\*)?/,
      action: {
        token: 'subtrack',
        next: 'root',
        transform(subtrack, content) {
          let repeat
          if (subtrack[1] !== undefined) {
            repeat = subtrack[1].slice(0, -1)
          } else {
            const pos = content.filter((e) => e.Type === 'BarLine' && e.Order[0] > 0)
            if (pos.length > 0) {
              repeat = Math.max(...pos.map((e) => Math.max(...e.Order)))
            } else {
              repeat = -1
            }
          }
          return {
            Type: 'Subtrack',
            Repeat: repeat,
            Content: content
          }
        }
      }
    },
    {
      regex: /^\)/,
      action: {
        token: '@pass',
        next: '@pop'
      }
    },
    {
      regex: /^[^{)]+/,
      action: {
        token: 'dyn',
        transform(match) {
          return {
            Type: 'Dyn',
            Content: match[0]
          }
        }
      }
    }
  ],
  root: [
    {
      regex: /^([0-7x%])([',#b]*)([A-Zac-wyz]*)([:>]*)([-_.=]*)(`*)/,
      action: {
        token: 'note',
        transform(note) {
          return {
            Type: 'Note',
            Pitches: [
              {
                Degree: note[1],
                PitOp: note[2] === undefined ? '' : note[2],
                Chord: note[3] === undefined ? '' : note[3],
                VolOp: note[4] === undefined ? '' : note[4]
              }
            ],
            PitOp: '',
            VolOp: '',
            DurOp: note[5] === undefined ? '' : note[5],
            Staccato: note[6] === undefined ? 0 : note[6].length
          }
        }
      }
    },
    {
      regex: /^\[(([0-7x%][',#A-Za-wyz:>]*)+)\]([',#b]*)([:>]*)([-_.=]*)(`*)/,
      action: {
        token: 'chord',
        transform(note) {
          return {
            Type: 'Note',
            Pitches: note[1].match(/[0-7x%][',#A-Za-wyz]*/g).map((pitch) => {
              const parts = pitch.match(/([0-7x%])([',#b]*)([ac-zA-Z]*)([:>]*)/)
              return {
                Degree: parts[1],
                PitOp: parts[2] === undefined ? '' : parts[2],
                Chord: parts[3] === undefined ? '' : parts[3],
                VolOp: parts[4] === undefined ? '' : parts[4]
              }
            }),
            PitOp: note[3] === undefined ? '' : note[3],
            DurOp: note[5] === undefined ? '' : note[5],
            VolOp: note[4] === undefined ? '' : note[4],
            Staccato: note[6] === undefined ? 0 : note[6].length
          }
        }
      }
    },
    {
      regex: /^(:\|\|:|:\|\||\|\|:|\||\\([\d,~\s])*:|\\|\/)/,
      action: {
        cases: {
          ':||:': {
            token: 'rEB'
          },
          ':||': {
            token: 'rE',
            transform() {
              return {
                Type: 'RepeatEnd'
              }
            }
          },
          '||:': {
            token: 'rB',
            transform() {
              return {
                Type: 'RepeatBegin'
              }
            }
          },
          '|': {
            token: 'ba',
            transform() {
              return {
                Type: 'BarLine',
                Skip: false,
                Order: [0],
                Overlay: false
              }
            }
          },
          '\\': {
            token: 'skip',
            transform() {
              return {
                Type: 'BarLine',
                Skip: true,
                Order: [0],
                Overlay: false
              }
            }
          },
          '/': {
            token: 'ol',
            transform() {
              return {
                Type: 'BarLine',
                Skip: false,
                Order: [0],
                Overlay: true
              }
            }
          },
          '@default': {
            token: 'pos',
            transform(pos) {
              let order = []
              if (pos[2] !== undefined) {
                const parts = pos[2].split(',')
                for (const part of parts) {
                  if (part.includes('~')) {
                    const [left, right] = part.split('~')
                    for (var i = left; i <= right; i++) {
                      order.push(i)
                    }
                  } else {
                    order.push(Number(part))
                  }
                }
              }
              return {
                Type: 'BarLine',
                Skip: false,
                Order: order,
                Overlay: false
              }
            }
          }
        }
      }
    },
    {
      regex: /^!/,
      action: {
        token: 'local',
        transform() {
          return {
            Type: 'LocalIndicator'
          }
        }
      }
    },
    {
      regex: /^\((\w+):([\d-]+)\)/,
      action: {
        token: 'sfunc',
        transform(match) {
          return {
            Type: 'Function',
            Name: match[1],
            Argument: [
              {
                Type: 'Number',
                Content: Number(match[2])
              }
            ]
          }
        }
      }
    },
    {
      regex: /^\((\d+)\/(\d+)\)/,
      action: {
        token: 'sfunc',
        transform(match) {
          return {
            Type: 'Function',
            Name: 'BarBeat',
            Argument: [
              {
                Type: 'Number',
                Content: Number(match[1])
              },
              {
                Type: 'Number',
                Content: Number(match[2])
              }
            ]
          }
        }
      }
    },
    {
      regex: /^\(1=([A-G',b#]+)\)/,
      action: {
        token: 'sfunc',
        transform(match) {
          return {
            Type: 'Function',
            Name: 'Key',
            Argument: [
              {
                Type: 'String',
                Content: match[1]
              }
            ]
          }
        }
      }
    },
    {
      regex: /^\((\d+)%\)/,
      action: {
        token: 'sfunc',
        transform(match) {
          return {
            Type: 'Function',
            Name: 'Vol',
            Argument: [
              {
                Type: 'Number',
                Content: Number(match[1])
              }
            ]
          }
        }
      }
    },
    {
      regex: /^\((\d+)\)/,
      action: {
        token: 'sfunc',
        transform(match) {
          return {
            Type: 'Function',
            Name: 'Spd',
            Argument: [
              {
                Type: 'Number',
                Content: Number(match[1])
              }
            ]
          }
        }
      }
    },
    {
      regex: /^\(([+-]\d+)\)/,
      action: {
        token: 'sfunc',
        transform(match) {
          return {
            Type: 'Function',
            Name: 'KeyShift',
            Argument: [
              {
                Type: 'Number',
                Content: Number(match[1])
              }
            ]
          }
        }
      }
    },
    {
      regex: /^\(/,
      action: {
        token: 'usfunc',
        next: 'Dynamic',
        transform(_, content) {
          return {
            Type: 'Sfunc',
            Content: content
          }
        }
      }
    },
    {
      regex: /^<\*([^]*?)\*>/,
      action: {
        token: 'comment',
        transform(content) {
          return {
            Type: 'Comment',
            Content: content[1]
          }
        }
      }
    },
    {
      regex: /^\[(\d+\.)+\]/,
      action: {
        token: 'volta',
        transform(volta) {
          return {
            Type: 'Volta',
            Order: volta[0].slice(1, -1).split('.').slice(0, -1).map((s) => Number(s))
          }
        }
      }
    },
    {
      regex: /^(\+|s|DC|DS|Fine)/,
      action: {
        token: 'repeats',
        transform(match) {
          const map = {
            '+': 'Coda',
            's': 'Segno',
            'DC': 'DaCapo',
            'DS': 'DaSegno',
            'Fine': 'Fine'
          }
          return {
            Type: map[match[1]]
          }
        }
      }
    },
    {
      regex: /^<([A-Za-z0-9]+:)?([A-Za-z0-9]+(\(.+?\))?(, *[A-Za-z0-9]+(\(.+?\))?)*)>/,
      action: {
        token: 'instr',
        transform(instrs) {
          const res = instrs[2].split(',').map((instr) => {
            const info = instr.match(/(\w+)(\(\d+%\))?/)
            return {
              Instrument: info[1],
              Proportion: info[2] === undefined ? null : Number(info[2].slice(1, -2)) / 100
            }
          })
          res.unshift(instrs[1] === undefined ? undefined : instrs[1].slice(0, -1))
          return res
        }
      }
    },
    {
      regex: /^@[a-zA-Z0-9]+/,
      action: {
        token: 'macroIndicator',
        transform(macro) {
          return {
            Type: 'Macrotrack',
            Name: macro[0].slice(1)
          }
        }
      }
    },
    {
      regex: /^([A-Za-z]\w*)\s*\(/,
      action: {
        token: 'func',
        next: 'Func',
        transform(func, content) {
          return {
            Type: 'Function',
            Name: func[1],
            Argument: content
          }
        }
      }
    },
    {
      regex: /^{(\d+\*)?/,
      action: {
        token: 'subtrack',
        next: 'root',
        transform(subtrack, content) {
          let repeat
          if (subtrack[1] !== undefined) {
            repeat = -subtrack[1].slice(0, -1)
          } else {
            const pos = content.filter((e) => e.Type === 'BarLine' && e.Order[0] > 0)
            if (pos.length > 0) {
              repeat = Math.max(...pos.map((e) => Math.max(...e.Order)))
            } else {
              repeat = -1
            }
          }
          return {
            Type: 'Subtrack',
            Repeat: repeat,
            Content: content
          }
        }
      }
    },
    {
      regex: /^}/,
      action: {
        token: '@pass',
        next: '@pop'
      }
    },
    {
      regex: /^&/,
      action: {
        token: 'pr',
        transform() {
          return {
            Type: 'PedalPress'
          }
        }
      }
    },
    {
      regex: /^\*/,
      action: {
        token: 'pr',
        transform() {
          return {
            Type: 'PedalRelease'
          }
        }
      }
    },
    {
      regex: /^\^/,
      action: {
        token: 'tie',
        transform() {
          return {
            Type: 'Tie'
          }
        }
      }
    },
    {
      regex: /./,
      action: {
        token: 'undef',
        transform(match) {
          return {
            Type: 'Undef',
            Content: match[0]
          }
        }
      }
    }
  ],
  Func: [
    {
      regex: /^\)/,
      action: {
        token: '@pass',
        next: '@pop'
      }
    },
    {
      regex: /^,\s*/,
      action: {
        token: '@pass'
      }
    },
    {
      regex: /^{(\d+\*)?/,
      action: {
        token: 'subtrack',
        next: 'root',
        transform(subtrack, content) {
          let repeat
          if (subtrack[1] !== undefined) {
            repeat = subtrack[1].slice(0, -1)
          } else {
            const pos = content.filter((e) => e.Type === 'BarLine' && e.Order[0] > 0)
            if (pos.length > 0) {
              repeat = Math.max(...pos.map((e) => Math.max(...e.Order)))
            } else {
              repeat = -1
            }
          }
          return {
            Type: 'Subtrack',
            Repeat: repeat,
            Content: content
          }
        }
      }
    },
    {
      regex: /^"([^"]*)"/,
      action: {
        token: 'string',
        transform(str) {
          return {
            Type: 'String',
            Content: str[1]
          }
        }
      }
    },
    {
      regex: /^\[/,
      action: {
        token: 'array',
        next: 'Array',
        transform(_, content) {
          return {
            Type: 'Array',
            Content: content
          }
        }
      }
    },
    {
      regex: /^-?(\d+\/\d+|\d+(\.\d+)?|Log2\(\d+\)([+-]\d+)?)/,
      action: {
        token: 'number',
        transform(num) {
          return {
            Type: 'Expression',
            Content: num[0]
          }
        }
      }
    }
  ],
  Array: [
    {
      regex: /^,\s*/,
      action: {
        token: '@pass'
      }
    },
    {
      regex: /^\]/,
      action: {
        token: '@pass',
        next: '@pop'
      }
    },
    {
      regex: /^{(\d+\*)?/,
      action: {
        token: 'subtrack',
        next: 'root',
        transform(subtrack, content) {
          let repeat
          if (subtrack[1] !== undefined) {
            repeat = subtrack[1].slice(0, -1)
          } else {
            const pos = content.filter((e) => e.Type === 'BarLine' && e.Order[0] > 0)
            if (pos.length > 0) {
              repeat = Math.max(...pos.map((e) => Math.max(...e.Order)))
            } else {
              repeat = -1
            }
          }
          return {
            Type: 'Subtrack',
            Repeat: repeat,
            Content: content
          }
        }
      }
    },
    {
      regex: /^"([^"]*)"/,
      action: {
        token: 'string',
        transform(str) {
          return {
            Type: 'String',
            Content: str[1]
          }
        }
      }
    },
    {
      regex: /^\[/,
      action: {
        token: 'array',
        next: 'Array',
        transform(_, content) {
          return {
            Type: 'Array',
            Content: content
          }
        }
      }
    },
    {
      regex: /^-?(\d+\/\d+|\d+(\.\d+)?|Log2\(\d+\)([+-]\d+)?)/,
      action: {
        token: 'number',
        transform(num) {
          return {
            Type: 'Expression',
            Content: num[0]
          }
        }
      }
    }
  ]
}

const libDef = [
  {
    regex: /^#\s*Chord([^]+?)\n(?=#)/,
    type: 'Chord',
    transform(chords) {
      const result = []
      const chordDefs = chords[1].split(/\n/)
      for (const chordDef of chordDefs) {
        const res = chordDef.match(/^([A-Za-z])\t+([^\t]+\t+)?([^\t]+)/)
        if (res === null) continue
        const parts = res[3].split(',')
        const pitches = []
        for (const part of parts) {
          const num = Number(part)
          if (Number.isNaN(num)) {
            const match = part.trim().match(/\[(.*?)\](-?\d+)?/)
            let delta
            if (match[2] === undefined) {
              delta = 0
            } else {
              delta = Number(match[2])
            }
            if (match[1] === '') {
              pitches.push([1, -1, delta])
            } else {
              const num2 = Number(match[1])
              if (Number.isNaN(num2)) {
                let [head, tail] = match[1].split(';')
                head = Number(head)
                if (tail === '') {
                  tail = -1
                } else {
                  tail = Number(tail)
                }
                pitches.push([head, tail, delta])
              } else {
                pitches.push([num2, num2, delta])
              }
            }
          } else {
            pitches.push([1, 1, num])
          }
        }
        result.push({
          Notation: res[1],
          Comment: res[2],
          Pitches: pitches
        })
      }
      return {
        Type: 'Chord',
        Data: result
      }
    }
  },
  {
    regex: /^#\s*Function([^]+?)\n(?=#)/,
    type: 'Function',
    transform(funcs) {
      return {
        Type: 'Function',
        Data: funcs[1]
      }
    }
  },
  {
    regex: /^#\s*Track([^]+?)\n(?=#)/,
    type: 'Macro',
    transform(macroAll, tok) {
      const result = []
      const macros = macroAll[0].match(/<:\w+:>[^]+?(?=<:(\w+):>|$)/g)
      for (const macro of macros) {
        const [, name, content] = macro.match(/<:(\w+):>([^]+)/)
        result.push({
          Name: name,
          Content: tok(content)
        })
      }
      return {
        Type: 'Track',
        Data: result
      }
    }
  },
  {
    regex: /^#\s*End/,
    type: '@terminal'
  }
]

module.exports = {
  sDef,
  langDef,
  libDef
}
