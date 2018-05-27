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
    const tracks = []
    let duration = 0
    this.Sections.forEach(section => {
      section.Tracks.forEach(track => {
        const data = track.Content.map(note => {
          return {
            Pitch: note.Pitch,
            Volume: note.Volume,
            Duration: note.Duration,
            StartTime: note.StartTime + duration
          }
        })
        let index = tracks.findIndex(t => t.Name === track.Name)
        if (index === -1) {
          index = tracks.length
          tracks.push({
            Name: track.Name,
            Meta: track.Meta,
            Settings: track.Settings,
            Content: []
          })
        }
        tracks[index].Content.push(...data)
        tracks[index].Meta.Duration = track.Meta.Duration + duration
      })
      duration += Math.max(...section.Tracks.map(track => track.Meta.Duration))
    })
    return tracks
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