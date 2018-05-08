class Markov {
  constructor(track) {
    this.firstNote = track[0]
    this.pitchSpace = {}
    let prevNotes = []
    for (const note of track) {
      if (!(note.Pitch in this.pitchSpace)) this.pitchSpace[note.Pitch] = {}
      if (prevNotes.length > 0 && prevNotes[0].StartTime < note.StartTime) {
        prevNotes.forEach(prev => {
          if (this.pitchSpace[prev.Pitch][note.Pitch] !== undefined) {
            this.pitchSpace[prev.Pitch][note.Pitch] += 1 / prevNotes.length
          } else {
            this.pitchSpace[prev.Pitch][note.Pitch] = 1 / prevNotes.length
          }
        })
        prevNotes = [note]
      } else {
        prevNotes.push(note)
      }
    }
    for (const src in this.pitchSpace) {
      let amount = 0, possibility = 0
      for (const dest in this.pitchSpace[src]) {
        amount += this.pitchSpace[src][dest]
      }
      for (const dest in this.pitchSpace[src]) {
        possibility += this.pitchSpace[src][dest] / amount
        this.pitchSpace[src][dest] = possibility
      }
    }
  }

  generate(count) {
    const result = [this.firstNote]
    const volume = this.firstNote.Volume
    const duration = this.firstNote.Duration
    let current = this.firstNote.Pitch
    for (let i = 1; i < count; i++) {
      const possibility = Math.random()
      for (const next in this.pitchSpace[current]) {
        if (possibility <= this.pitchSpace[current][next]) {
          result.push({
            Type: 'Note',
            Pitch: next,
            Volume: volume,
            Duration: duration
          })
          current = next
          break
        }
      }
    }
    return result
  }
}

module.exports = Markov

