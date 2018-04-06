const { tokenizeTrack } = require('./util')
const LibTokenizer = require('./LibTokenizer')

class Tokenizer {
  static isHeadTrack(track) {
    const heads = ['Volta', 'RepeatBegin', 'RepeatEnd', 'Setting', 'Coda', 'Segno', 'DaCapo', 'DaSegno', 'Fine']
    const settings = ['ConOct', 'Vol', 'Spd', 'Key', 'Oct', 'KeyOct', 'Beat', 'Bar', 'BarBeat', 'Dur', 'Acct', 'Light', 'Seg', 'Port', 'Trace', 'FadeIn', 'FadeOut', 'Rev', 'Ferm', 'Stac']
    return track.every((element) => {
      return heads.includes(element.Type) || (element.Type === 'Function' && settings.includes(element.Name))
    })
  }

  constructor(content, langDef, sDef, libDef) {
    this.content = content.replace(/\r\n/g, '\n')
    this.include = []
    this.sections = []
    this.baseIndex = 0
    this.sectionIndex = []
    this.trackIndex = []
    this.comments = []
    this.langDef = langDef
    this.libDef = libDef
    this.sDef = sDef
    this.libs = undefined
    this.result = {
      Comments: undefined,
      Library: [],
      Sections: []
    }
  }

  tokenize() {
    this.extractHeader()
    this.split()
    for (let i = 0, length = this.sections.length; i < length; i++) {
      const sec = {
        Type: 'Section',
        Comments: this.comments[i],
        Settings: [],
        Tracks: []
      }
      let pointer = 0
      for (const track of this.sections[i]) {
        const tra = tokenizeTrack(track, this.langDef, this.sDef)
        if (tra[0] instanceof Array) {
          const instr = tra.shift()
          const ID = instr.shift()
          sec.Tracks.push({
            ID,
            Instruments: instr,
            Content: tra
          })
          pointer += 1
        } else if (tra[0].Type === 'LocalIndicator') {
          sec.Settings.push(...tra.slice(1))
          this.trackIndex[i].splice(pointer, 1)
        } else if (Tokenizer.isHeadTrack(tra)) {
          this.result.Sections.push(...tra)
          this.trackIndex[i].splice(pointer, 1)
        } else {
          sec.Tracks.push({
            ID: null,
            Instruments: [],
            Content: tra
          })
          pointer += 1
        }
      }
      if (sec.Settings.length === 0 && sec.Tracks.length === 0) continue
      this.result.Sections.push(sec)
    }
    return this.result
  }

  split() {
    const pattern = /(^(\/\/.*)?\n){2,}/mg
    let match
    let lastIndex = 0
    const secs = []
    while ((match = pattern.exec(this.content)) !== null) {
      if (match.index === 0) {
        continue
      } else {
        const tempSec = this.content.slice(lastIndex, match.index)
        if (tempSec.trim() !== '') {
          secs.push(tempSec)
          this.sectionIndex.push(lastIndex)
        }
        lastIndex = match.index
      }
    }
    const tempSec = this.content.slice(lastIndex)
    if (tempSec.trim() !== '') {
      secs.push(tempSec)
      this.sectionIndex.push(lastIndex)
    }
    for (let i = 0, length = secs.length; i < length; i++) {
      let baseIndex = 0
      const comments = []
      const tras = secs[i].replace(/^\/\/(.*)/gm, (str, comment) => {
        baseIndex += str.length
        comments.push(comment)
        return ''
      })
      const temp = this.splitSection(tras, baseIndex)
      this.comments.push(comments)
      this.sections.push(temp.tracks)
      this.trackIndex.push(temp.trackIndex)
    }
  }

  splitSection(content, baseIndex) {
    const pattern = /(^\n)+/mg
    let match
    let lastIndex = 0
    const tracks = []
    const trackIndex = []
    while ((match = pattern.exec(content)) !== null) {
      if (match.index === 0) {
        continue
      } else {
        const tempTrack = content.slice(lastIndex, match.index)
        if (tempTrack.trim() !== '') {
          tracks.push(tempTrack)
          trackIndex.push(lastIndex + baseIndex)
        }
        lastIndex = match.index
      }
    }
    const tempTrack = content.slice(lastIndex)
    if (tempTrack.trim() !== '') {
      tracks.push(tempTrack)
      trackIndex.push(lastIndex + baseIndex)
    }
    return {
      tracks,
      trackIndex
    }
  }

  extractHeader() {
    this.content = this.content.replace(/^\n*(\/\/.*\n)+\n*/, (str) => {
      this.baseIndex += str.length
      this.result.Comments = str.trim().replace(/^\/\//mg, '').split('\n')
      return ''
    })
    if (this.content.startsWith('#')) {
      this.content = this.content.replace(/^#\s*Include\s+"([^"\n]+)\n"/gm, (str, name) => {
        this.baseIndex += str.length
        this.result.Library.push({
          Type: 'Package',
          Path: name,
          Content: new LibTokenizer(name, false).tokenize()
        })
        return ''
      })
      const end = this.content.match(/^#\s*End\n/m)
      if (end !== null) {
        const libLen = end.index + end[0].length
        this.baseIndex += libLen
        const libstr = this.content.slice(0, libLen)
        this.result.Library.push(...new LibTokenizer(libstr, this.libDef).tokenize())
        this.content = this.content.slice(libLen)
      }
    }
  }
}

module.exports = Tokenizer
