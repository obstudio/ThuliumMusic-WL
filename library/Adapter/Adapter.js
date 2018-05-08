class TmAdapter {
  constructor(data) {
    this.Sections = data
  }

  theme(...spec) {
    const result = []
    let duration = 0
    this.Sections.slice(...spec).forEach(section => {
      result.push(...section.Tracks[0].Content.map(note => {
        note.StartTime += duration
        return note
      }))
      duration += section.Tracks[0].Meta.Duration
    })
    return result
  }
}

module.exports = TmAdapter