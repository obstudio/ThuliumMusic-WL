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
            const dur = content[release].StartTime - content[press].StartTime
            content.splice(release, 1)
            content.splice(press, 1)
            content.slice(press, release).forEach((tok) => {
                tok.Duration = final - tok.StartTime
            })
        }
    }

    constructor(track, sectionSettings, libraries, isSubtrack = false) {
        this.isSubtrack = isSubtrack
        this.ID = track.ID
        this.Instruments = track.Instruments
        this.Libraries = libraries
        this.Content = track.Content
        this.Settings = sectionSettings.extend()
        this.Context = {
            afterTie: false,
            locator: {
                index: -1,
                bar: 0
            },
            notesBeforeTie: [],
            startTime: 0,
            pitchQueue: [],
            warnings: []
        }
    }

    pushError(errorType, args, useLocator = true) {
        let location = []
        if (useLocator) {
            location.push(Object.assign({}, this.Context.locator))
        }
        this.Context.warnings.push(new TmError(errorType, location, args))
    }

    parseTrack() {
        this.preprocess()
        if (this.isSubtrack) {
            const trackResult = this.parseTrackContent()
            TrackParser.processPedal(trackResult)
            return [trackResult]
        } else {
            if (this.Instruments.every((instrument) => instr.includes(instrument.Instrument))) {
                currentType = 0
            } else if (this.Instruments.every((instrument) => drum.includes(instrument.Instrument))) {
                currentType = 1
            } else {
                currentType = 0
            }
            const trackResult = this.parseTrackContent()
            TrackParser.processPedal(trackResult)
            if (this.Instruments.length === 0) {
                this.Instruments.push({
                    Instrument: 'Piano',
                    Proportion: 1
                })
            }
            if (trackResult.Meta.Duration < this.Settings.FadeIn || trackResult.Meta.Duration < this.Settings.FadeOut) {
                this.pushError(TmError.Types.Track.FadeOverLong, { Actual: [this.Settings.FadeIn, this.Settings.FadeOut] }, false)
            }
            if (!this.Instruments.every((instrument) => instrument.Instrument === '' || instr.includes(instrument.Instrument) || drum.includes(instrument.Instrument))) {
                this.pushError(TmError.Types.Track.Instrument, { Actual: this.Instruments }, false)
            }
            return this.Instruments.map((instrument) => {
                const meta = Object.assign({}, trackResult.Meta, { Warnings: trackResult.Meta.Warnings.slice() })
                if (instrument.Proportion === null) {
                    instrument.Proportion = 1
                }
                if (instrument.Instrument === '') {
                    instrument.Instrument = 'Piano'
                }
                return {
                    Instrument: instrument.Instrument,
                    ID: this.ID ? `${this.ID}#${instrument.Instrument}` : instrument.Instrument,
                    Meta: meta,
                    Content: trackResult.Content.map((note) => {
                        let vol = note.Volume * instrument.Proportion
                        if (vol > 1) {
                            meta.Warnings.push(new TmError(TmError.Types.Note.VolumeLimit, [], { Expected: 1, Actual: vol })) // FIXME: index
                            vol = 1
                        }
                        delete note.__oriDur
                        return Object.assign({}, note, { Volume: vol })
                    })
                }
            })
        }
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

    parseTrackContent() {
        const result = []
        let subtrack
        let rightIncomplete
        let leftIncomplete = 0
        let leftFirst = true

        for (const token of this.Content) {
            this.Context.locator.index += 1
            switch (token.Type) {
                case 'FUNCTION':
                case 'Subtrack':
                    if (token.Type === 'FUNCTION') {
                        subtrack = this.Libraries.FunctionPackage.applyFunction(this, token)
                        if (subtrack === undefined) {
                            break
                        }
                    } else {
                        subtrack = new SubtrackParser(token, this.Settings, this.Libraries, this.Context.pitchQueue).parseTrack()
                    }
                    subtrack.Content.forEach((tok) => {
                        if (tok.Type === 'Note') {
                            tok.StartTime += this.Context.startTime
                        }
                    })
                    this.Context.pitchQueue.push(...subtrack.Meta.PitchQueue)
                    this.Context.startTime += subtrack.Meta.Duration
                    this.Context.notesBeforeTie = subtrack.Meta.NotesBeforeTie
                    this.Context.afterTie = subtrack.Meta.AfterTie
                    this.Context.warnings.push(...subtrack.Meta.Warnings.map((warning) => {
                        warning.pos.unshift(Object.assign({}, this.Context.locator))
                        return warning
                    }))
                    if (subtrack.Meta.Single) {
                        if (leftFirst) {
                            leftIncomplete += subtrack.Meta.Incomplete[0]
                            if (this.isLegalBar(leftIncomplete)) {
                                leftFirst = false
                                rightIncomplete = 0
                            }
                        } else {
                            rightIncomplete += subtrack.Meta.Incomplete[0]
                            if (this.isLegalBar(rightIncomplete)) {
                                rightIncomplete = 0
                            }
                        }
                    } else {
                        if (leftFirst) {
                            leftIncomplete += subtrack.Meta.Incomplete[0]
                            leftFirst = false
                            rightIncomplete = subtrack.Meta.Incomplete[1]
                            if (this.isLegalBar(rightIncomplete)) {
                                rightIncomplete = 0
                            }
                        } else {
                            rightIncomplete += subtrack.Meta.Incomplete[0]
                            if (!this.isLegalBar(rightIncomplete)) {
                                this.pushError(TmError.Types.Track.BarLength, { Expected: this.Settings.Bar, Actual: rightIncomplete })
                            }
                            rightIncomplete = subtrack.Meta.Incomplete[1]
                            if (this.isLegalBar(rightIncomplete)) {
                                rightIncomplete = 0
                            }
                        }
                    }
                    result.push(...subtrack.Content)
                    break
                case 'Note':
                    this.Context.notesBeforeTie = this.parseNote(token)
                    if (leftFirst) {
                        leftIncomplete += this.parseBeat(token)
                    } else {
                        rightIncomplete += this.parseBeat(token)
                    }
                    result.push(...this.Context.notesBeforeTie.filter((note) => result.indexOf(note) === -1))
                    break
                case 'Tie':
                    this.Context.afterTie = true
                    break
                case 'BarLine':
                    leftFirst = false
                    if (token.Terminal !== true) {
                        if (!this.isLegalBar(rightIncomplete)) {
                            this.pushError(TmError.Types.Track.BarLength, { Expected: this.Settings.Bar, Actual: rightIncomplete })
                        }
                        rightIncomplete = 0
                    }
                    if (token.Overlay) {
                        this.Context.startTime = 0
                    }
                    this.Context.locator.bar += 1
                    break
                case 'PedalPress':
                case 'PedalRelease':
                    result.push({
                        Type: token.Type,
                        StartTime: this.Context.startTime
                    })
                    break
                case 'Undefined':
                    this.pushError(TmError.Types.Track.Undefined, { Actual: token })
                    break
                case 'Clef':
                case 'Whitespace':
                    break
            }
        }
        const returnObj = {
            Content: result,
            Meta: {
                Warnings: this.Context.warnings,
                PitchQueue: this.isSubtrack ? this.Context.pitchQueue.slice(this.oriPitchQueueLength) : this.Context.pitchQueue,
                FadeIn: this.Settings.FadeIn,
                FadeOut: this.Settings.FadeOut,
                Duration: this.Context.startTime,
                Single: leftFirst,
                Incomplete: [leftIncomplete, rightIncomplete],
                NotesBeforeTie: this.Context.notesBeforeTie,
                AfterTie: this.Context.afterTie
            }
        }
        if (!this.isSubtrack) {
            returnObj.Meta.toJSON = function toJson() {
                return {
                    FadeIn: this.FadeIn,
                    FadeOut: this.FadeOut,
                    Duration: this.Duration,
                    Warnings: this.Warnings
                }
            }
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
            if (this.Context.pitchQueue.length >= this.Settings.Trace) {
                const delta = this.parseDeltaPitch(note.PitOp)
                const queue = this.Context.pitchQueue[this.Context.pitchQueue.length - this.Settings.Trace]
                pitchQueue.push(...queue)
                pitches.push(...[].concat(...queue.map((pitch) => this.Settings.Key.map((key) => key - this.Settings.Key[0] + pitch + delta))))
                volumes.push(...[].concat(...new Array(queue.length).fill(this.getVolume(note.VolOp))))
            } else {
                this.pushError(TmError.Types.Note.NoPrevious, { Expected: this.Settings.Trace, Actual: this.Context.pitchQueue.length })
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
        if (new Set(pitches).size !== pitches.length) {
            this.pushError(TmError.Types.Note.Reduplicate, { Actual: pitches })
        }
        if (pitchQueue.length > 0) {
            this.Context.pitchQueue.push(pitchQueue.slice(0))
        }

        const result = []
        // merge pitches with previous ones if tie exists
        if (this.Context.afterTie) {
            this.Context.afterTie = false
            this.Context.notesBeforeTie.forEach((prevNote) => {
                const index = pitches.indexOf(prevNote.Pitch)
                if (index === -1 || prevNote.Volume !== volumes[index]) return
                result.push(prevNote)
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
                StartTime: this.Context.startTime
            })
        }
        this.Context.startTime += duration
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
            const all = new Array(length).fill(1)
            operator.forEach(([head, tail, delta]) => {
                if (head < 0) {
                    if (head < -length) {
                        this.pushError(TmError.Types.Note.ChordRange, { Expected: -length, Actual: head })
                    }
                    head += length + 1
                } else if (head > length) {
                    this.pushError(TmError.Types.Note.ChordRange, { Expected: length, Actual: head })
                }
                if (tail < 0) {
                    if (tail < -length) {
                        this.pushError(TmError.Types.Note.ChordRange, { Expected: -length, Actual: tail })
                    }
                    tail += length + 1
                } else if (tail > length) {
                    this.pushError(TmError.Types.Note.ChordRange, { Expected: length, Actual: tail })
                }
                for (let i = head; i <= tail; i++) {
                    all[i - 1] = 0
                }
                res.push(...pitches.slice(head - 1, tail).map((pitch) => pitch + delta))
            })
            this.pushError(TmError.Types.Note.ChordOverride, {})
            if (!all.every((e) => e === 0)) this.pushError(TmError.Types.Note.ChordOverride, {})
            return res
        }, [0])
    }

    parsePitch(pitch, base) {
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
    constructor(track, sectionSettings, libraries, pitchQueue) {
        super(track, sectionSettings, libraries, true)
        this.Repeat = track.Repeat
        if (pitchQueue === undefined) {
            this.Context.pitchQueue = []
            this.oriPitchQueueLength = 0
        } else {
            this.Context.pitchQueue = pitchQueue.slice()
            this.oriPitchQueueLength = pitchQueue.length
        }
    }

    parseTrack() {
        this.preprocess()
        const trackResult = this.parseTrackContent(this.Content)
        return trackResult
    }

    preprocess() {
        this.mergeMacro()
        if (this.Repeat > 0) {
            this.Content.forEach((token, index) => {
                if (token.Skip === true) {
                    this.Context.warnings.push(new TmError(TmError.Types.Track.UnexpCoda, [{ index }], { Actual: token }))
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
                    this.Context.warnings.push(new TmError(TmError.Types.Track.UnexpVolta, [{ index }], { Actual: token }))
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
                    this.Context.warnings.push(new TmError(TmError.Types.Track.MultiCoda, [{ index }], {}))
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
