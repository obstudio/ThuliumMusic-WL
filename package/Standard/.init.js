module.exports = {

    _fill_(src, dest) {
        const result = [];
        const duration = dest.Meta.Duration;
        for (let i = 0; i < duration; i += src.Meta.Duration) {
            src.Content.forEach(note => {
                if (note.StartTime + i < duration) {
                    result.push(Object.assign({}, note, {StartTime: note.StartTime + i}))
                }
            })
        }
        return Object.assign(dest, {Content: result});
    },

    Tremolo1(expr, subtrack) {
        /**** (^%1-)&2 ****/
        const dest = this.ParseTrack(subtrack);
        const time = Math.pow(2, -expr) * 60 / this.Settings.Speed;
        const scale = time / dest.Meta.Duration;
        const srcContent = data.Content.map(note => Object.assign({}, note, {
            StartTime: note.StartTime * scale,
            Duration: note.Duration * scale
        }));
        return this.GetFunction('_fill_')({Meta: {Duration: time}, Content: srcContent}, dest);
    },

    Tremolo2(expr, subtrack1, subtrack2) {
        /**** &2(^%1=)&3 ****/
        const ts = [this.ParseTrack(subtrack1), this.ParseTrack(subtrack2)]
        const pow = Math.pow(2, -(expr)) * 60 / this.Settings.Speed
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
        let BarFirst, BarLast
        let single
        if (ts[0].Meta.BarCount == 1 && ts[1].Meta.BarCount == 1) {
            BarFirst = ts[1].Meta.BarFirst
            single = true
        } else if (ts[0].Meta.BarCount == 1) {
            BarFirst = ts[1].Meta.BarFirst
            BarLast = ts[1].Meta.BarLast
            single = false
        } else if (ts[1].Meta.BarCount == 1) {
            BarFirst = ts[0].Meta.BarFirst
            BarLast = ts[1].Meta.BarFirst
            single = false
        } else {
            BarFirst = ts[0].Meta.BarFirst
            BarLast = ts[1].Meta.BarLast
            single = false
        }
        return {
            Content: result,
            Warnings: [],
            Meta: {
                Duration: ts[1].Meta.Duration,
                BarFirst: BarFirst,
                BarLast: BarLast,
                Single: single,
                PitchQueue: [...ts[0].Meta.PitchQueue, ...ts[1].Meta.PitchQueue],
                NotesBeforeTie: ts[(num - 1) % 2].Meta.NotesBeforeTie
            }
        }
    },

    Tuplet(expr, subtrack) {
        /**** (^!1~)&2 ****/
        const scale = Math.pow(2, Math.floor(Math.log2(expr))) / expr
        const t = this.ParseTrack(subtrack, this.Settings.extend({ Bar: this.Settings.Bar / scale }))
        t.Content.forEach((note) => {
            note.__oriDur *= scale
            note.Duration *= scale
            note.StartTime *= scale
        })
        t.Meta.Duration *= scale
        if (t.Meta.BarCount == 1) {
            t.Meta.BarFirst *= scale
        } else {
            t.Meta.BarFirst *= scale
            t.Meta.BarLast *= scale
        }
        return t
    },

    Portamento(subtrack1, subtrack2) {
        /**** &1~&2 ****/
        const t1 = this.ParseTrack(subtrack1)
        const t2 = this.ParseTrack(subtrack2)

        const pitch1 = t1.Content[t1.Content.length - 1].Pitch
        const pitch2 = t2.Content[0].Pitch
        const duration = t1.Meta.Duration
        const port = this.Settings.getOrSetDefault('Port', 6)
        const num = duration * port * this.Settings.Speed / 60
        const step = (pitch2 - pitch1) / (num - 1)
        const pitches = []
        for (let i = 0; i < num; i++) {
            pitches.push(Math.round(pitch1 + step * i))
        }
        const result = pitches.map((pitch, index) => {
            return {
                Type: 'Note',
                Pitch: pitch,
                Volume: t2.Content[0].Volume,
                Duration: 1 / port * 60 / this.Settings.Speed,
                StartTime: index / port * 60 / this.Settings.Speed,
                __oriDur: 1 / port * 60 / this.Settings.Speed
            }
        })
        result.push(...t2.Content.map((note) => {
            note.StartTime += duration
            return note
        }))
        const single = t1.Meta.BarCount == 1 && t2.Meta.BarCount == 1
        let BarFirst, BarLast
        if (single) {
            BarFirst = t1.Meta.BarFirst + t2.Meta.BarFirst
        } else if (t1.Meta.BarCount == 1) {
            BarFirst = t1.Meta.BarFirst + t2.Meta.BarFirst
            BarLast = t2.Meta.BarLast
        } else if (t2.Meta.BarCount == 1) {
            BarFirst = t1.Meta.BarFirst
            BarLast = t1.Meta.BarLast + t2.Meta.BarFirst
        } else {
            BarFirst = t1.Meta.BarFirst
            BarLast = t2.Meta.BarLast
        }

        return {
            Content: result,
            Warnings: [],
            Meta: {
                Duration: duration,
                BarFirst: BarFirst,
                BarLast: BarLast,
                Single: single,
                PitchQueue: [...t1.Meta.PitchQueue, ...t2.Meta.PitchQueue],
                NotesBeforeTie: [result[result.length - 1]]
            }
        }
    },

    GraceNote(subtrack1, subtrack2) {
        /**** (^&1\^)&2 ****/
        const t1 = this.ParseTrack(subtrack1)
        const t2 = this.ParseTrack(subtrack2)
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
            note.__oriDur = actualDur
        })
        const total = actualDur * num
        t2.Content.forEach((note) => {
            note.StartTime += total
            note.Duration -= total
            note.__oriDur -= total
        })
        return {
            Content: [...t1.Content, ...t2.Content],
            Warnings: [],
            Meta: t2.Meta
        }
    },

    Appoggiatura(subtrack1, subtrack2) {
        /**** &1(\^^&2) ****/
        const t1 = this.ParseTrack(subtrack1)
        const t2 = this.ParseTrack(subtrack2)
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
            note.__oriDur -= total
        })
        t2.Content.forEach((note) => {
            note.Duration = actualDur
            note.__oriDur = actualDur
            note.StartTime *= dur
            note.StartTime += t1.Meta.Duration - total
        })
        t1.Meta.NotesBeforeTie = t2.Meta.NotesBeforeTie
        return {
            Content: [...t1.Content, ...t2.Content],
            Warnings: [],
            Meta: t1.Meta
        }
    },

    Fermata(subtrack) {
        /**** (.)&1 ****/
        const t = this.ParseTrack(subtrack)
        const ferm = this.Settings.getOrSetDefault('Ferm', 2)
        t.Content.forEach((note) => {
            note.Duration *= ferm
            note.__oriDur *= ferm
            note.StartTime *= ferm
        })
        t.Meta.Duration *= ferm
        return t
    },

    Arpeggio(subtrack) {
        /**** \$&1 ****/
        const t = this.ParseTrack(subtrack)
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
                temp.__oriDur = actualDur
                for (const note of sum) {
                    result.push(Object.assign({}, note, { StartTime: actualDur * index }))
                }
            } else {
                t.Content.forEach((note) => {
                    note.StartTime += actualDur * index
                    note.Duration -= actualDur * index
                    note.__oriDur -= actualDur * index
                })
                result.push(...t.Content)
            }
            return sum
        }, [])
        return Object.assign(t, { Content: result })
    },

    Con(octave, scale = 1) {
        if (octave === 0) {
            this.Settings.Key = [this.Settings.Key[0]]
            this.Settings.Volume = [this.Settings.Volume[0]]
        } else {
            this.Settings.Key = [this.Settings.Key[0], this.Settings.Key[0] + octave * 12]
            this.Settings.Volume = [this.Settings.Volume[0], this.Settings.Volume[0] * scale]
        }
        // this.Settings.assignSetting('ConOct', octave, (octave) => Number.isInteger(octave))
        // this.Settings.assignSetting('ConOctVolume', volumeScale, (volume) => volume >= 0)
    },

    Vol(volume) {
        /**** (^!1\%) ****/
        if (volume instanceof Array) {
            this.Settings.Volume = volume
        } else {
            const delta = (volume / 100) / this.Settings.Volume[0]
            for (var i = 0, length = this.Settings.Volume.length; i < length; i++) {
                this.Settings.Volume[i] *= delta
            }
        }
        // this.Settings.assignSetting('Volume', volume / 100, (volume) => volume >= 0)
    },

    Key(key) {
        /**** (1=$1) ****/
        let delta
        if (typeof key === 'string') {
            const match = arguments[0].match(/^((#|b)\2*)?([A-G])(('|,)\5*)?/)
            const Tonality = {
                'C': 0,
                'G': 7,
                'D': 2,
                'A': 9,
                'E': 4,
                'B': -1,
                'F': 5
            }
            delta = Tonality[match[3]] + (match[2] === undefined ? 0 : (match[2] === '#' ? match[1].length : -match[1].length)) +
                (match[5] === undefined ? 0 : (match[5] === '\'' ? (12 * match[4].length) : (-12 * match[4].length))) - this.Settings.Key[0]
        } else {
            delta = key - this.Settings.Key[0]
        }
        for (var i = 0, length = this.Settings.Key.length; i < length; i++) {
            this.Settings.Key[i] += delta
        }
        // this.Settings.assignSetting('Key', key, (key) => Number.isInteger(key))
    },

    KeyShift(delta) {
        /**** (!1) ****/
        for (var i = 0, length = this.Settings.Key.length; i < length; i++) {
            this.Settings.Key[i] += delta
        }
    },

    Oct() {
        if (arguments.length === 0) return
        if (!(arguments[0] instanceof Array)) {
            const delta = (arguments[0] - Math.floor((this.Settings.Key[0] + 2) / 12)) * 12
            for (let i = 0, length = this.Settings.Key.length; i < length; i++) {
                this.Settings.Key[i] += delta
            }
        } else {
            const tonality = (this.Settings.Key[0] - 2) % 12
            this.Settings.Key = arguments[0].map((oct) => tonality + oct * 12)
            if (arguments.length >= 2) this.Settings.Volume = arguments[1]
        }
    },

    Spd(speed) {
        /**** (^!1) ****/
        this.Settings.assignSetting('Speed', speed, (speed) => speed > 0)
    },

    BarBeat(bar, beat) {
        /**** (^!1/^!2) ****/
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

    Stac(rest, index = 1) {
        this.Settings.assignSettingAtIndex('Stac', index, rest, (rest) => rest >= 0 && rest <= 1)
    }
}
