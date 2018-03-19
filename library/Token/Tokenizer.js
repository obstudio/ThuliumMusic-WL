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
        Type: 'FUNCTION',
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
        Type: 'FUNCTION',
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
        Type: 'FUNCTION',
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
        Type: 'FUNCTION',
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
        Type: 'Subtrack'
      }
    ],
    transform(match) {
      return {
        Type: 'FUNCTION',
        Name: 'Fermata',
        Simplified: true,
        Argument: [
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
          /(\d+)~/
        ]
      },
      {
        Type: 'Subtrack'
      }
    ],
    transform(match) {
      return {
        Type: 'FUNCTION',
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
          { Type: 'Subtrack' },
          /^\^/
        ]
      },
      {
        Type: 'Note'
      }
    ],
    transform(match) {
      return {
        Type: 'FUNCTION',
        Name: 'GraceNote',
        Simplified: true,
        Argument: [
          match[0].Content[0],
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
          /^\^$/,
          { Type: 'Subtrack' }
        ]
      }
    ],
    transform(match) {
      return {
        Type: 'FUNCTION',
        Name: 'Appoggiatura',
        Simplified: true,
        Argument: [
          {
            Type: 'Subtrack',
            Content: [match[0]],
            Repeat: -1
          },
          match[1].Content[1]
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
      regex: /^([0-7x%])([',#b]*)([A-Zac-wyz]*)([',#b]*)([-_.=]*)(`*)([:>]*)/,
      action: {
        token: 'note',
        transform(note) {
          return {
            Type: 'Note',
            Pitches: [
              {
                Degree: note[1],
                PitOp: note[2] === undefined ? '' : note[2],
                Chord: note[3] === undefined ? '' : note[3]
              }
            ],
            PitOp: note[4] === undefined ? '' : note[4],
            DurOp: note[5] === undefined ? '' : note[5],
            VolOp: note[7] === undefined ? '' : note[7],
            Staccato: note[6] === undefined ? 0 : note[6].length
          }
        }
      }
    },
    {
      regex: /^\[(([0-7x%][',#A-Za-wyz:>]*)+)\]([',#b]*)([-_.=]*)(`*)([:>]*)/,
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
            DurOp: note[4] === undefined ? '' : note[4],
            VolOp: note[6] === undefined ? '' : note[6],
            Staccato: note[5] === undefined ? 0 : note[5].length
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
        transform(_, content) {
          return {
            Type: 'LocalIndicator',
            Settings: content
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
            Type: 'FUNCTION',
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
            Type: 'FUNCTION',
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
            Type: 'FUNCTION',
            Name: 'KeyOct',
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
            Type: 'FUNCTION',
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
            Type: 'FUNCTION',
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
      regex: /^\(/,
      action: {
        token: 'sfunc',
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
        transform (match) {
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
      regex: /^<([A-Za-z0-9]+:)?([A-Za-z0-9]+(\(.+?\))?)(,[A-Za-z0-9]+(\(.+?\))?)*>/,
      action: {
        token: 'instr',
        transform(instrs) { // For ID
          const res = instrs[0].slice(1, -1).split(',').map((instr) => {
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
            Type: 'FUNCTION',
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
    regex: /^#\s*Chord([^]+?)\r?\n(?=#)/,
    type: 'Chord',
    transform(chords) {
      const result = []
      const chordDefs = chords[1].split(/\r?\n/)
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
    regex: /^#\s*Function([^]+?)\r?\n(?=#)/,
    type: 'Function',
    transform(funcs) {
      return {
        Type: 'Function',
        Data: []
      }
    }
  },
  {
    regex: /^#\s*Track([^]+?)\r?\n(?=#)/,
    type: 'Macro',
    transform(macroAll) {
      const result = []
      const macros = macroAll[0].match(/<\*\w+\*>[^]+?(?=<\*(\w+)\*>|$)/g)
      for (const macro of macros) {
        const [, name, content] = macro.match(/<\*(\w+)\*>([^]+)/)
        result.push({
          Name: name,
          Content: Tokenizer.tokenizeTrack(content)
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

export default class Tokenizer {
  /**
   *
   * @param {string} track
   */
  static tokenizeTrack(track) {
    track = track.trim()
    const stateStore = [[]]
    const states = ['root']
    let depth = 0
    let pointer = 0
    while (pointer < track.length) {
      const temp = track.slice(pointer)
      const slice = temp.trim()
      let matched = false
      pointer += temp.length - slice.length
      const patterns = langDef[states[depth]]

      for (let index = 0; index < patterns.length; index++) {
        const element = patterns[index]
        const match = slice.match(element.regex)
        if (match === null) continue
        let action
        matched = true
        if ('cases' in element.action) {
          if (match[0] in element.action.cases) {
            action = element.action.cases[match[0]]
          } else {
            action = element.action.cases['@default']
          }
        } else {
          action = element.action
        }
        if ('next' in action) {
          if (action.token !== '@pass') {
            stateStore[depth].push((content) => action.transform(match, content))
          }
          if (action.next === '@pop') {
            depth -= 1
            const state = stateStore.pop()
            for (let i = 0; i < sDef.length; i++) {
              const s = sDef[i]
              for (let j = 0; j <= state.length - s.pat.length; j++) {
                let isMatch = true
                for (let k = 0; k < s.pat.length; k++) {
                  if (s.pat[k].Type === state[j + k].Type) {
                    if (s.pat[k].Type === 'Sfunc') {
                      for (let l = 0; l < s.pat[k].Content.length; l++) {
                        if (s.pat[k].Content[l] instanceof RegExp) {
                          if (state[j + k].Content[l].Type !== 'Dyn' || !s.pat[k].Content[l].test(state[j + k].Content[l].Content)) {
                            isMatch = false
                            break
                          }
                        } else if (s.pat[k].Content[l].Type !== state[j + k].Content[l].Type) {
                          isMatch = false
                          break
                        }
                      }
                    } else if (s.pat[k].Type === 'Undef' && s.pat[k].Content !== state[j + k].Content) {
                      isMatch = false
                      break
                    }
                  } else {
                    isMatch = false
                    break
                  }
                }
                if (!isMatch) continue
                state.splice(j, s.pat.length, s.transform(state.slice(j, j + s.pat.length)))
              }
            }
            states.pop()
            stateStore[depth].push(stateStore[depth].pop()(state))
          } else {
            stateStore.push([])
            states.push(action.next)
            depth += 1
          }
        } else {
          if (action.token !== '@pass') {
            stateStore[depth].push(action.transform(match))
          }
        }
        pointer += match[0].length
        break
      }
      if (!matched) {
        // stateStore.push(track.charAt(pointer))
        // pointer += 1
      }
    }
    const state = stateStore[0]
    for (let i = 0; i < sDef.length; i++) {
      const s = sDef[i]
      for (let j = 0; j <= state.length - s.pat.length; j++) {
        let isMatch = true
        for (let k = 0; k < s.pat.length; k++) {
          if (s.pat[k].Type === state[j + k].Type) {
            if (s.pat[k].Type === 'Sfunc') {
              for (let l = 0; l < s.pat[k].Content.length; l++) {
                if (s.pat[k].Content[l] instanceof RegExp) {
                  if (state[j + k].Content[l].Type !== 'Dyn' || !s.pat[k].Content[l].test(state[j + k].Content[l].Content)) {
                    isMatch = false
                    break
                  }
                } else if (s.pat[k].Content[l].Type !== state[j + k].Content[l].Type) {
                  isMatch = false
                  break
                }
              }
            } else if (s.pat[k].Type === 'Undef' && s.pat[k].Content !== state[j + k].Content) {
              isMatch = false
              break
            }
          } else {
            isMatch = false
            break
          }
        }
        if (!isMatch) continue
        state.splice(j, s.pat.length, s.transform(state.slice(j, j + s.pat.length)))
      }
    }
    return state
  }

  static isHeadTrack(track) {
    const heads = ['Volta', 'RepeatBegin', 'RepeatEnd', 'Setting', 'Coda', 'Segno', 'DaCapo', 'DaSegno', 'Fine']
    const settings = ['ConOct', 'Vol', 'Spd', 'Key', 'Oct', 'KeyOct', 'Beat', 'Bar', 'BarBeat', 'Dur', 'Acct', 'Light', 'Seg', 'Port', 'Trace', 'FadeIn', 'FadeOut', 'Rev', 'Ferm', 'Stac']
    return track.every((element) => {
      return heads.includes(element.Type) || (element.Type === 'FUNCTION' && settings.includes(element.Name))
    })
  }
  /**
   * Construct a tokenizer
   * @param {string} content Tm string to tokenize
   */
  constructor(content) {
    this.content = content
    this.include = []
    this.sections = []
    this.libs = undefined
    this.result = {
      Library: [],
      Sections: []
    }
  }

  tokenize() {
    this.regularize()
    this.removeComment()
    this.extractHeader()
    this.split()
    for (const section of this.sections) {
      const sec = {
        Type: 'Section',
        Settings: [],
        Tracks: []
      }
      for (const track of section) {
        const tra = Tokenizer.tokenizeTrack(track)
        if (tra[0] instanceof Array) {
          const instr = tra.shift()
          const ID = instr.shift()
          sec.Tracks.push({
            ID,
            Instruments: instr,
            Content: tra
          })
        } else if (tra[0].Type === 'LocalIndicator') {
          sec.Settings.push(...tra.slice(1))
        } else if (Tokenizer.isHeadTrack(tra)) {
          this.result.Sections.push(...tra)
        } else {
          sec.Tracks.push({
            ID: null,
            Instruments: [{
              Instrument: '',
              Proportion: null
            }],
            Content: tra
          })
        }
      }
      if (sec.Settings.length === 0 && sec.Tracks.length === 0) continue
      this.result.Sections.push(sec)
    }
    return this.result
  }

  split() {
    const secs = this.content.split(/(\r?\n){3,}/)
    for (let i = 0, length = secs.length; i < length; i++) {
      const sec = secs[i]
      const section = []
      if (sec !== '' && sec !== '\n' && sec !== '\r\n') {
        const tras = sec.split(/\r?\n\r?\n/)
        for (let j = 0, length2 = tras.length; j < length2; j++) {
          const tra = tras[j]
          if (tra !== '') {
            section.push(tra.replace(/\r?\n/, ''))
          }
        }
        this.sections.push(section)
      }
    }
  }

  regularize() {
    // this.content = this.content.replace(/[ \t\f\v]+(\n|$)/g, '$1')
  }

  removeComment() {
    this.content = this.content.replace(/\/\/.*$/gm, '')
  }

  extractHeader() {
    this.content = this.content.replace(/^#\s*Include\s+"([^"]+)"/gm, (str, name) => {
      this.result.Library.push({
        Type: 'Package',
        Path: name,
        Content: new LibTokenizer(name, false).tokenize()
      })
      return ''
    })
    const end = this.content.match(/^#\s*End/m)
    if (end !== null) {
      const parts = this.content.split(end[0])
      this.result.Library.push(...new LibTokenizer(parts[0] + end[0]).tokenize())
      this.content = parts[1]
    }
  }
}

class LibTokenizer {
  constructor(content, internal = true) {
    this.inc = []
    this.content = undefined
    if (internal) {
      this.content = content.trim()
    } else {
      this.load(content)
    }
  }

  load(path) {
    const content = '' // TODO: load via http or fs
    this.content = content.replace(/^#\s*Include\s+"([^"]+)"/gm, (str, name) => {
      this.inc.push(name)
      return ''
    })
  }

  tokenize() {
    const result = []
    let pointer = 0
    for (const inc of this.inc) {
      result.push({
        Type: 'Package',
        Path: name,
        Content: new LibTokenizer(inc, false).tokenize()
      })
    }
    while (pointer < this.content.length) {
      const temp = this.content.slice(pointer)
      const slice = temp.trim()
      pointer += temp.length - slice.length

      for (let index = 0; index < libDef.length; index++) {
        const element = libDef[index]
        const match = slice.match(element.regex)
        if (match === null) continue
        if (element.type === '@terminal') return result
        result.push(element.transform(match))
        pointer += match[0].length
        break
      }
    }
    return result
  }
}
