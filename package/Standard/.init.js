const { SubtrackParser } = require('../../src/TrackParser')

module.exports = {
    Tremolo1(expr, subtrack) {
        const t = new SubtrackParser(subtrack, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
        const pow = Math.pow(2, -expr) * 60 / this.Settings.Speed
        const num = Math.round(t.Meta.Duration / pow)
        const result = []
        const length = t.Content.length
        for (let i = 0; i < num; i++) {
            const startTime = i * pow
            for (let j = 0; j < length; j++) {
                result.push(Object.assign({}, t.Content[j], { StartTime: startTime, Duration: pow }))
            }
        }

        return {
            Content: result,
            Meta: t.Meta
        }
    },

    Tremolo2(expr, subtrack1, subtrack2) {
        const ts = [new SubtrackParser(subtrack1, this.Settings, this.Libraries, this.pitchQueue).parseTrack(), new SubtrackParser(subtrack2, this.Settings, this.Libraries, this.pitchQueue).parseTrack()]
        const pow = Math.pow(2, -expr) * 60 / this.Settings.Speed
        const num = Math.round(ts[1].Meta.Duration / pow)
        const lengths = ts.map((t) => t.Content.length)
        const result = []
        for (let i = 0; i < num; i++) {
            const startTime = i * pow
            const index = i % 2
            for (let j = 0; j < lengths[index]; j++) {
                result.push(Object.assign({}, ts[index].Content[j], { StartTime: startTime, Duration: pow }))
            }
        }
        return {
            Content: result,
            Meta: {
                Duration: ts[1].Meta.Duration,
                Incomplete: ts[1].Meta.Incomplete,
                Single: true,
                Warnings: [],
                PitchQueue: [...ts[0].Meta.PitchQueue, ...ts[1].Meta.PitchQueue],
                NotesBeforeTie: ts[(num - 1) % 2].Meta.NotesBeforeTie
            }
        }
    },

    Tuplet(expr, subtrack) {
        const scale = Math.pow(2, Math.floor(Math.log2(expr))) / expr
        const t = new SubtrackParser(subtrack, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
        t.Content.forEach((note) => {
            note.Duration *= scale
            note.StartTime *= scale
        })
        t.Meta.Duration *= scale
        return t
    },

    Portamento(subtrack1, subtrack2) {
        const t1 = new SubtrackParser(subtrack1, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
        const t2 = new SubtrackParser(subtrack2, this.Settings, this.Libraries, this.pitchQueue).parseTrack()

        const pitch1 = t1.Content[0].Pitch
        const pitch2 = t2.Content[0].Pitch
        const duration = t1.Meta.Duration
        const port = this.Settings.getOrSetDefault('Port', 6)
        const num = duration * port
        const step = (pitch2 - pitch1) / (num - 1)
        const pitches = []
        for (let i = 0; i < port; i++) {
            pitches.push(Math.round(pitch1 + step * i))
        }

        const result = pitches.map((pitch, index) => {
            return {
                Type: 'Note',
                Pitch: pitch,
                Volume: t2.Content[0].Volume,
                Duration: 1 / port,
                StartTime: index / port
            }
        })

        return {
            Content: result,
            Meta: {
                Duration: duration,
                Incomplete: [duration],
                Single: true,
                Warnings: [],
                PitchQueue: [...t1.Meta.PitchQueue, ...t2.Meta.PitchQueue],
                NotesBeforeTie: [result[result.length - 1]]
            }
        }
    },

    GraceNote(subtrack1, subtrack2) {
        const t1 = new SubtrackParser(subtrack1, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
        const t2 = new SubtrackParser(subtrack2, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
        const num = subtrack1.Content.length
        let dur
        const appo = this.Settings.getOrSetDefault('Seg', 1 / 4)
        if (num <= 4) {
            dur = appo / 4
        } else {
            dur = appo / num
        }
        const actualDur = dur * Math.pow(2, -this.Settings.Duration) * 60 / this.Settings.Speed
        t1.Content.forEach((note) => {
            note.Duration = actualDur
            note.StartTime *= dur
        })
        const total = actualDur * num
        t2.Content.forEach((note) => {
            note.StartTime += total
            note.Duration -= total
        })
        return {
            Content: [...t1.Content, ...t2.Content],
            Meta: t2.Meta
        }
    },

    Appoggiatura(subtrack1, subtrack2) {
        const t1 = new SubtrackParser(subtrack1, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
        const t2 = new SubtrackParser(subtrack2, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
        const num = subtrack2.Content.length
        let dur
        const appo = this.Settings.getOrSetDefault('Seg', 1 / 4)
        if (num <= 4) {
            dur = appo / 4
        } else {
            dur = appo / num
        }
        const actualDur = dur * Math.pow(2, -this.Settings.Duration) * 60 / this.Settings.Speed
        const total = actualDur * num
        t1.Content.forEach((note) => {
            note.Duration -= total
        })
        t2.Content.forEach((note) => {
            note.Duration = actualDur
            note.StartTime *= dur
            note.StartTime += t1.Content[0].Duration
        })

        return {
            Content: [...t1.Content, ...t2.Content],
            Meta: t1.Meta
        }
    },

    Fermata(subtrack) {
        const t = new SubtrackParser(subtrack, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
        const ferm = this.Settings.getOrSetDefault('Ferm', 2)
        t.Content.forEach((note) => {
            note.Duration *= ferm
            note.StartTime *= ferm
        })
        t.Meta.Duration *= ferm
        return t
    },

    Arpeggio(subtrack) {
        const t = new SubtrackParser(subtrack, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
        const num = t.Content.length - 1
        let dur
        const appo = this.Settings.getOrSetDefault('Seg', 1 / 4)
        if (num <= 4) {
            dur = appo / 4
        } else {
            dur = appo / num
        }
        const actualDur = dur * Math.pow(2, -this.Settings.Duration) * 60 / this.Settings.Speed
        const result = []
        t.Content.reduce((sum, cur, index) => {
            if (index < num) {
                const temp = Object.assign({}, cur)
                sum.push(temp)
                temp.Duration = actualDur
                for (const note of sum) {
                    result.push(Object.assign({}, note, { StartTime: actualDur * index }))
                }
            } else {
                t.Content.forEach((note) => {
                    note.StartTime += actualDur * index
                    note.Duration -= actualDur * index
                })
                result.push(...t.Content)
            }
            return sum
        }, [])
        return Object.assign(t, { Content: result })
    },

    ConOct(octave) {
        if (octave === 0) {
            this.Settings.Key = [this.Settings.Key[0]]
        } else {
            this.Settings.Key = [this.Settings.Key[0], this.Settings.Key[0] + octave * 12]
        }
        // this.Settings.assignSetting('ConOct', octave, (octave) => Number.isInteger(octave))
        // this.Settings.assignSetting('ConOctVolume', volumeScale, (volume) => volume >= 0)
    },
    Vol(volume) {
        const delta = volume / this.Settings.Volume[0]
        for (var i = 0, length = this.Settings.Volume.length; i < length; i++) {
            this.Settings.Volume[i] *= delta
        }
        // this.Settings.assignSetting('Volume', volume / 100, (volume) => volume >= 0)
    },
    Spd(speed) {
        this.Settings.assignSetting('Speed', speed, (speed) => speed > 0)
    },
    Key(key) {
        const delta = key - this.Settings.Key[0]
        for (var i = 0, length = this.Settings.Key.length; i < length; i++) {
            this.Settings.Key[i] += delta
        }
        // this.Settings.assignSetting('Key', key, (key) => Number.isInteger(key))
    },
    Oct(oct) {
        const delta = (oct - Math.floor((this.Settings.Key[0] + 2) / 12)) * 12
        for (var i = 0, length = this.Settings.Key.length; i < length; i++) {
            this.Settings.Key[i] += delta
        }
        // this.Settings.assignSetting('Octave', oct, (octave) => Number.isInteger(octave))
    },
    KeyOct(keyOct) {
        const match = keyOct.match(/^((#|b)\2*)?([A-G])(('|,)\5*)?/)

        const Tonality = {
            'C': 0,
            'G': 7,
            'D': 2,
            'A': 9,
            'E': 4,
            'B': -1,
            'F': 5
        }
        let delta = Tonality[match[3]] + (match[2] === undefined ? 0 : (match[2] === '#' ? match[1].length : -match[1].length))
            + (match[5] === undefined ? 0 : (match[5] === '\'' ? (12 * match[4].length) : (-12 * match[4].length))) - this.Settings.Key[0]
        for (var i = 0, length = this.Settings.Key.length; i < length; i++) {
            this.Settings.Key[i] += delta
        }
        // this.Settings.assignSetting('Key', Tonality[key], (key) => Number.isInteger(key))
        // this.Settings.assignSetting('Octave', oct, (octave) => Number.isInteger(octave))
    },
    Beat(beat) {
        this.Settings.assignSetting('Beat', beat, (beat) => beat > 0 && Number.isInteger(Math.log2(beat)))
    },
    Bar(bar) {
        this.Settings.assignSetting('Bar', bar, (bar) => bar > 0 && Number.isInteger(bar))
    },
    BarBeat(bar, beat) {
        this.Settings.assignSetting('Bar', bar, (bar) => bar > 0 && Number.isInteger(bar))
        this.Settings.assignSetting('Beat', beat, (beat) => beat > 0 && Number.isInteger(Math.log2(beat)))
    },
    Dur(scale) {
        this.Settings.assignSetting('Duration', scale, () => true)
    },
    Acct(scale) {
        this.Settings.assignSetting('Accent', scale, (scale) => scale > 1)
    },
    Light(scale) {
        this.Settings.assignSetting('Light', scale, (scale) => scale < 1 && scale > 0)
    },
    Seg(r) {
        this.Settings.assignSetting('Seg', r, (r) => r > 0)
    },
    Port(r) {
        this.Settings.assignSetting('Port', r, (r) => r > 0)
    },
    Trace(count) {
        this.Settings.assignSetting('Trace', count, (count) => count > 0 && count <= 4 && Number.isInteger(count))
    },
    FadeIn(time) {
        this.Settings.assignSetting('FadeIn', time, (time) => time >= 0)
    },
    FadeOut(time) {
        this.Settings.assignSetting('FadeOut', time, (time) => time >= 0)
    },
    Rev(r) {
        this.Settings.assignSetting('Rev', r, () => true)
    },
    Ferm(ferm) {
        this.Settings.assignSetting('Ferm', ferm, (ferm) => ferm > 1)
    },
    Stac(restProportion, index = 1) {
        if (typeof restProportion !== 'number') throw new TypeError('Non-numeric value passed in as Stac')
        if (!((restProportion) => restProportion >= 0 && restProportion <= 1)(restProportion)) throw new RangeError('Stac out of range')
        if (![0, 1, 2].includes(index)) throw new RangeError('Stac index out of range')
        this.Settings.Stac[index] = restProportion
    }
}
