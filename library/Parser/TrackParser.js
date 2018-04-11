const TmError = require('./Error')
const instrDict = require('../Config/Instrument.json')
const drumDict = require('../Config/Percussion.json')

let currentType = 0
const instr = Object.keys(instrDict)
const drum = Object.keys(drumDict)

class TrackParser {
    static processPedal(trackResult) {
        const content = trackResult.Content
        let press
        let release
        // eslint-disable-next-line no-constant-condition
        while (true) {
            press = content.findIndex((tok) => tok.Type === 'PedalPress')
            release = content.findIndex((tok) => tok.Type === 'PedalRelease')
            if (press === -1) break
            if (release === -1) {
                content.splice(press, 1)
                content.slice(press).forEach((tok) => {
                    tok.Duration = trackResult.Meta.Duration - tok.StartTime
                })
                break
            }
            while (release < press) {
                content.splice(release, 1)
                release = content.findIndex((tok) => tok.Type === 'PedalRelease')
                press -= 1
            }
            const final = content[release].StartTime
            content.splice(release, 1)
            content.splice(press, 1)
            content.slice(press, release).forEach((tok) => {
                tok.Duration = final - tok.StartTime
            })
        }
    }

    static isDup(arr) {
        const length = arr.length
        let i = -1
        while (i++ < length) {
            for (let j = i + 1; j < length; ++j) {
                if (arr[i] === arr[j]) {
                    return true
                }
            }
        }
        return false
    }

    constructor(track, instrument, sectionSettings, libraries) {
        this.ID = track.ID
        this.Instrument = instrument
        this.Libraries = libraries
        this.Content = track.Content
        this.Settings = sectionSettings.extend()
        this.Meta = {
            Index: -1,
            NotesBeforeTie: [],
            PitchQueue: [],
            // pitchFirst: 第一个音符,
            // pitchLast: 最后一个音符,
            FadeIn: this.Settings.FadeIn, // FIXME: seems buggy
            FadeOut: this.Settings.FadeOut, // FIXME: seems buggy
            BarFirst: 0,
            BarLast: 0,
            Duration: 0,
            BarCount: 0, // 总小节线数
            BeatCount: 0,
            TieLeft: false,
            TieRight: false
        }
        this.Result = []
        this.Warnings = []
    }

    pushError(errorType, args, useLocator = true) {
        this.Warnings.push(new TmError(errorType, useLocator ? {
            Bar: this.Meta.BarCount,
            Index: this.Meta.Index
        } : null, args))
    }

    parseTrack() {
        this.preprocess()
        if (instr.includes(this.Instrument.Name)) {
            currentType = 0
        } else if (drum.includes(this.Instrument.Name)) {
            currentType = 1
        } else {
            currentType = 0
        }
        const trackResult = this.parseTrackContent()
        TrackParser.processPedal(trackResult)
        if (trackResult.Meta.Duration < this.Settings.FadeIn || trackResult.Meta.Duration < this.Settings.FadeOut) {
            this.pushError(TmError.Types.Track.FadeOverLong, { Actual: [this.Settings.FadeIn, this.Settings.FadeOut] }, false)
        }
        if (!(instr.includes(this.Instrument.Name) || drum.includes(this.Instrument.Name))) {
            this.pushError(TmError.Types.Track.Instrument, { Actual: this.Instrument }, false)
        }
        trackResult.Instrument = this.Instrument.Name
        trackResult.ID = this.ID ? `${this.ID}#${this.Instrument.Name}` : this.Instrument.Name
        return trackResult
    }

    mergeMacro() {
        let length = this.Content.length
        let pointer = 0
        while (pointer < length) {
            const token = this.Content[pointer]
            if (token.Type === 'Macrotrack' && token.Name in this.Libraries.Track) {
                const macro = this.Libraries.Track[token.Name]
                this.Content.splice(pointer, 1, ...macro)
                length += macro.length - 1
            } else {
                pointer += 1
            }
        }
    }

    preprocess() {
        this.Content = [...this.Instrument.Spec, ...this.Content]
        this.mergeMacro()
        if (this.Content.length === 1) return
        const last = this.Content.pop()
        const last2 = this.Content.pop()
        if (last.Type === 'BarLine' && last2.Type === 'BarLine') {
            this.Content.push(Object.assign({}, last2, { Terminal: true }))
        } else {
            if (last.Type === 'BarLine') {
                this.Content.push(last2)
                this.Content.push(Object.assign({}, last, { Terminal: true }))
            } else {
                this.Content.push(last2)
                this.Content.push(last)
            }
        }
    }

    mergeMeta(dest, src) {
        dest.Meta.PitchQueue.push(...src.Meta.PitchQueue)
        dest.Meta.Duration += src.Meta.Duration
        dest.Meta.NotesBeforeTie = src.Meta.NotesBeforeTie
        dest.Meta.TieLeft = src.Meta.TieLeft
        dest.Warnings.push(...src.Warnings.map((warning) => {
            warning.pos.unshift(Object.assign({}, {
                Bar: dest.Meta.BarCount,
                Index: dest.Meta.Index
            }))
            return warning
        }))
        if (src.Meta.BarCount === 0) {
            if (dest.Meta.BarCount === 0) {
                dest.Meta.BarFirst += src.Meta.BarFirst
                if (dest.isLegalBar(dest.Meta.BarFirst)) {
                    dest.Meta.BarCount += 1
                }
            } else {
                dest.Meta.BarLast += src.Meta.BarFirst
                if (dest.isLegalBar(dest.Meta.BarLast)) {
                    dest.Meta.BarLast = 0
                }
            }
        } else {
            if (dest.Meta.BarCount === 0) {
                dest.Meta.BarFirst += src.Meta.BarFirst
                dest.Meta.BarCount += 1
                dest.Meta.BarLast = src.Meta.BarLast
                if (dest.isLegalBar(dest.Meta.BarLast)) {
                    dest.Meta.BarLast = 0
                }
            } else {
                dest.Meta.BarLast += src.Meta.BarFirst // problematic
                if (!dest.isLegalBar(dest.Meta.BarLast)) {
                    dest.pushError(TmError.Types.Track.BarLength, {
                        Expected: dest.Settings.Bar,
                        Actual: dest.Meta.BarFirst
                    })
                }
                dest.Meta.BarLast = src.Meta.BarLast
                if (dest.isLegalBar(dest.Meta.BarLast)) {
                    dest.Meta.BarLast = 0
                }
            }
        }
    }

    parseTrackContent() {
        for (const token of this.Content) {
            this.Meta.Index += 1
            switch (token.Type) {
                case 'Function':
                case 'Subtrack': {
                    let subtrack
                    if (token.Type === 'Function') {
                        subtrack = this.Libraries.Package.applyFunction(this, token)
                        if (subtrack === undefined) {
                            break
                        }
                    } else {
                        subtrack = new SubtrackParser(token, this.Settings, this.Libraries, this.Meta).parseTrack()
                    }
                    subtrack.Content.forEach((tok) => {
                        if (tok.Type === 'Note') {
                            tok.StartTime += this.Meta.Duration
                        }
                    })
                    this.mergeMeta(this, subtrack)
                    this.Result.push(...subtrack.Content)
                    break
                }
                case 'Note': {
                    const notes = this.parseNote(token)
                    const beat = this.parseBeat(token)
                    if (this.Meta.BarCount === 0) {
                        this.Meta.BarFirst += beat
                    } else {
                        this.Meta.BarLast += beat
                    }
                    this.Result.push(...notes.filter((note) => this.Result.indexOf(note) === -1))
                    break
                }
                case 'Tie':
                    this.Meta.TieLeft = true
                    break
                case 'BarLine':
                    this.Meta.BarCount += 1
                    if (token.Terminal !== true) {
                        if (!this.isLegalBar(this.Meta.BarLast)) {
                            this.pushError(TmError.Types.Track.BarLength, { Expected: this.Settings.Bar, Actual: this.Meta.BarFirst })
                        }
                        this.Meta.BarLast = 0
                    } else if (this.isLegalBar(this.Meta.BarLast)) {
                        this.Meta.BarLast = 0
                    }
                    if (token.Overlay) {
                        this.Meta.Duration = 0
                    }
                    break
                case 'PedalPress':
                case 'PedalRelease':
                    this.Result.push({
                        Type: token.Type,
                        StartTime: this.Meta.Duration
                    })
                    break
                case 'Undefined':
                case 'Sfunc':
                    throw new Error()
                // this.pushError(TmError.Types.Track.Undefined, { Actual: token })
                // break
                case 'Clef':
                case 'Comment':
                case 'Whitespace':
                    break
            }
        }
        const returnObj = {
            Content: this.Result,
            Warnings: this.Warnings,
            Settings: this.Settings,
            Meta: Object.assign(this.Meta, { PitchQueue: this instanceof SubtrackParser ? this.Meta.PitchQueue.slice(this.oriPitchQueueLength) : this.Meta.PitchQueue })
        }
        return returnObj
    }

    isLegalBar(bar) {
        return bar === undefined || Math.abs(bar - this.Settings.Bar) < 0.0000001 || bar === 0
    }

    parseNote(note) {
        const pitches = []
        const pitchQueue = []
        const volumes = []
        const beat = this.parseBeat(note)
        const duration = beat * 60 / this.Settings.Speed
        const actualDuration = duration * (1 - this.Settings.Stac[note.Staccato])

        // calculate pitch array and record it for further trace
        if (note.Pitches.length === 1 && note.Pitches[0].Degree === '%') {
            if (this.Meta.PitchQueue.length >= this.Settings.Trace) {
                const delta = this.parseDeltaPitch(note.PitOp || '')
                const queue = this.Meta.PitchQueue[this.Meta.PitchQueue.length - this.Settings.Trace]
                pitchQueue.push(...queue)
                pitches.push(...[].concat(...queue.map((pitch) => this.Settings.Key.map((key) => key - this.Settings.Key[0] + pitch + delta))))
                volumes.push(...[].concat(...new Array(queue.length).fill(this.getVolume(note.VolOp + note.Pitches[0].VolOp))))
            } else {
                this.pushError(TmError.Types.Note.NoPrevious, { Expected: this.Settings.Trace, Actual: this.Meta.PitchQueue.length })
            }
        } else {
            for (const pitch of note.Pitches) {
                if ((currentType === 0 && pitch.Degree === 'x') || (currentType === 1 && pitch.Degree !== 'x' && pitch.Degree !== '0')) {
                    this.pushError(TmError.Types.Note.ScaleDegree, { Actual: pitch.Degree })
                }
                if (pitch.Degree === '0') continue
                if (pitch.Degree === 'x') {
                    pitches.push(null)
                    volumes.push(this.Settings.Volume[0] * note.VolOp.split('').reduce((sum, cur) => sum * cur === '>' ? this.Settings.Accent : cur === ':' ? this.Settings.Light : 1, 1))
                } else if (pitch.Chord === '') {
                    const temp = this.parsePitch(pitch, note.PitOp)
                    pitchQueue.push(temp[0])
                    pitches.push(...temp)
                    volumes.push(...this.getVolume(note.VolOp + pitch.VolOp))
                } else {
                    const basePitch = this.parsePitch(pitch, note.PitOp)
                    const chords = this.parseChord(pitch)
                    pitchQueue.push(...chords.map(subPitch => subPitch + basePitch[0]))
                    pitches.push(...[].concat(...chords.map((subPitch) => basePitch.map((delta) => subPitch + delta))))
                    volumes.push(...[].concat(...new Array(chords.length).fill(this.getVolume(note.VolOp + pitch.VolOp))))
                }
            }
        }
        if (!volumes.every((vol) => vol <= 1)) {
            this.pushError(TmError.Types.Note.VolumeLimit, { Actual: volumes })
            volumes.forEach((vol, index, arr) => {
                if (vol > 1) {
                    arr[index] = 1
                }
            })
        }
        if (TrackParser.isDup(pitches)) {
            this.pushError(TmError.Types.Note.Reduplicate, { Actual: pitches })
        }
        if (pitchQueue.length > 0) {
            this.Meta.PitchQueue.push(pitchQueue.slice(0))
        }

        const result = []
        const notesBeforeTie = []
        // merge pitches with previous ones if tie exists
        if (this.Meta.TieLeft) {
            this.Meta.TieLeft = false
            this.Meta.NotesBeforeTie.forEach((prevNote) => {
                const index = pitches.indexOf(prevNote.Pitch)
                if (index === -1 || prevNote.Volume !== volumes[index]) return
                notesBeforeTie.push(prevNote)
                prevNote.__oriDur += actualDuration
                prevNote.Duration = prevNote.__oriDur
                pitches.splice(index, 1)
                volumes.splice(index, 1)
            })
        }
        for (var index = 0, length = pitches.length; index < length; index++) {
            result.push({
                Type: 'Note',
                Pitch: pitches[index],
                Volume: volumes[index],
                Duration: actualDuration,
                __oriDur: duration,
                Beat: beat,
                StartTime: this.Meta.Duration,
                StartBeat: this.Meta.BeatCount
            })
        }
        this.Meta.NotesBeforeTie = notesBeforeTie.concat(result)
        this.Meta.Duration += duration
        this.Meta.BeatCount += beat
        return result
    }

    getVolume(volOp) {
        const scale = volOp.split('').reduce((sum, cur) => sum * (cur === '>' ? this.Settings.Accent : (cur === ':' ? this.Settings.Light : 1)), 1)
        const total = this.Settings.Key.length
        const vol = this.Settings.Volume.length
        return [...this.Settings.Volume, ...new Array(total - vol).fill(this.Settings.Volume[vol - 1])].map((v) => v * scale)
    }

    parseChord(pitch) {
        return pitch.Chord.split('').reduce((pitches, chord) => {
            const operator = this.Libraries.Chord[chord]
            const res = []
            const length = pitches.length
            const all = new Array(length).fill(true)
            operator.forEach(([head, tail, delta]) => {
                if (head < 0) {
                    if (head < -length) {
                        this.pushError(TmError.Types.Note.ChordRange, { Expected: -length, Actual: head })
                    }
                    head += length
                } else if (head >= length) {
                    this.pushError(TmError.Types.Note.ChordRange, { Expected: length - 1, Actual: head })
                }
                if (tail < 0) {
                    if (tail < -length) {
                        this.pushError(TmError.Types.Note.ChordRange, { Expected: -length, Actual: tail })
                    }
                    tail += length
                } else if (tail >= length) {
                    this.pushError(TmError.Types.Note.ChordRange, { Expected: length - 1, Actual: tail })
                }
                for (let i = head; i <= tail; i++) {
                    all[i] = false
                }
                res.push(...pitches.slice(head, tail + 1).map((pitch) => pitch + delta))
            })
            if (!all.every((e) => !e)) this.pushError(TmError.Types.Note.ChordOverride, {})
            return res
        }, [0])
    }

    parsePitch(pitch, base) {
        base = base || ''
        const delta = this.parseDeltaPitch(base) + TrackParser.pitchDict[pitch.Degree] + this.parseDeltaPitch(pitch.PitOp)
        return this.Settings.Key.map((key) => key + delta)
    }

    parseDeltaPitch(pitchOperators) {
        return pitchOperators.split('').reduce((sum, cur) => sum + TrackParser.pitchOperatorDict[cur], 0)
    }

    parseBeat(note) {
        let duration = 1
        let pointer = 0
        let dotCount = 0
        const length = note.DurOp.length
        while (pointer < length) {
            const char = note.DurOp.charAt(pointer)
            switch (char) {
                case '=':
                    duration /= 4
                    pointer += 1
                    break
                case '-':
                    duration += 1
                    pointer += 1
                    break
                case '_':
                    duration /= 2
                    pointer += 1
                    break
                case '.':
                    dotCount = 1
                    pointer += 1
                    while (note.DurOp.charAt(pointer) === '.') {
                        dotCount += 1
                        pointer += 1
                    }
                    duration *= 2 - Math.pow(2, -dotCount)
                    break
            }
        }
        return duration * Math.pow(2, -this.Settings.Duration)
    }
}
TrackParser.pitchDict = { 1: 0, 2: 2, 3: 4, 4: 5, 5: 7, 6: 9, 7: 11 }
TrackParser.pitchOperatorDict = { '#': 1, 'b': -1, '\'': 12, ',': -12 }

class SubtrackParser extends TrackParser {
    constructor(track, settings, libraries, { PitchQueue: pitchQueue, NotesBeforeTie: notesBeforeTie, TieLeft: tieLeft }) {
        super(track, null, settings, libraries)
        this.Repeat = track.Repeat
        if (pitchQueue === undefined) {
            this.Meta.PitchQueue = []
            this.oriPitchQueueLength = 0
        } else {
            this.Meta.PitchQueue = pitchQueue.slice()
            this.oriPitchQueueLength = pitchQueue.length
        }
        if (notesBeforeTie !== null) {
            this.Meta.NotesBeforeTie = notesBeforeTie
        }
        if (tieLeft !== null) {
            this.Meta.TieLeft = tieLeft
        }
    }

    parseTrack() {
        this.preprocess()
        const trackResult = this.parseTrackContent(this.Content)
        return trackResult
    }

    preprocess() {
        this.mergeMacro()
        if (this.Repeat === undefined) this.Repeat = -1;
        if (this.Repeat > 0) {
            this.Content.forEach((token, index) => {
                if (token.Skip === true) {
                    this.Warnings.push(new TmError(TmError.Types.Track.UnexpCoda, { index }, { Actual: token }))
                }
            })
            const temp = []
            const repeatArray = this.Content.filter((token) => token.Type === 'BarLine' && token.Order[0] !== 0)
            const defaultOrder = repeatArray.find((token) => token.Order.length === 0)
            if (defaultOrder !== undefined) {
                const order = [].concat(...repeatArray.map((token) => token.Order))
                for (let i = 1; i < this.Repeat; i++) {
                    if (order.indexOf(i) === -1) defaultOrder.Order.push(i)
                }
            }
            for (let i = 1; i <= this.Repeat; i++) {
                let skip = false
                for (const token of this.Content) {
                    if (token.Type !== 'BarLine' || token.Order[0] === 0) {
                        if (!skip) {
                            temp.push(token)
                        }
                    } else if (token.Order.indexOf(i) === -1) {
                        skip = true
                    } else {
                        skip = false
                        temp.push(token)
                    }
                }
                temp.push({
                    Type: 'BarLine',
                    Skip: false,
                    Order: [0]
                })
            }
            this.Content = temp
        } else {
            this.Content.forEach((token, index) => {
                if (token.Order instanceof Array && (token.Order.length !== 1 || token.Order[0] !== 0)) {
                    this.Warnings.push(new TmError(TmError.Types.Track.UnexpVolta, { index }, { Actual: token }))
                }
            })
            if (this.Repeat !== -1 && this.Content.length >= 1) {
                const last = this.Content[this.Content.length - 1]
                if (last.Type !== 'BarLine') {
                    this.Content.push({
                        Type: 'BarLine',
                        Skip: false,
                        Order: [0]
                    })
                }
            }
            const skip = this.Content.findIndex((tok) => tok.Skip === true)
            for (let index = skip + 1, length = this.Content.length; index < length; index++) {
                if (this.Content[index].Skip === true) {
                    this.Warnings.push(new TmError(TmError.Types.Track.MultiCoda, { index }, {}))
                }
            }
            let temp
            if (skip === -1) {
                temp = new Array(-this.Repeat).fill(this.Content)
            } else {
                temp = new Array(-this.Repeat - 1).fill(this.Content)
                temp.push(this.Content.slice(0, skip))
            }
            this.Content = [].concat(...temp)
        }
    }
}

module.exports = {
    TrackParser,
    SubtrackParser
}
