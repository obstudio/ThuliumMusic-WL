class LibTokenizer {
  constructor(content, libDef, internal = true) {
    this.inc = []
    this.libDef = libDef
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

      for (let index = 0; index < this.libDef.length; index++) {
        const element = this.libDef[index]
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

module.exports = LibTokenizer
