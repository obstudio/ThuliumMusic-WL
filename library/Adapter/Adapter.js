class TmAdapter {
  constructor(data, ...spec) {
    if (spec.length === 0) {
      this.Sections = data
    } else if (spec.length === 1) {
      if (spec[0] > 0) {
        this.Sections = [ data[spec[0] - 1] ]
      } else {
        this.Sections = [ data[data.length + spec[0]] ]
      }
    } else {
      const begin = spec[0] > 0 ? spec[0] - 1 : data.length + spec[0]
      const end = spec[1] > 0 ? spec[1] : data.length + spec[1] + 1
      this.Sections = data.slice(begin, end)
    }
    this.Tracks = []
  }

  adapt() {
    this.Tracks = this.Sections[0].Tracks
    return this.Tracks.map(track => {
      return {
        Instrument: track.Instrument,
        Content: track.Content,
        Effects: track.Effects
      }
    })
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