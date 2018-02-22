const { BarLengthError, DupChordError, TraceError, VolumeError } = require('./Error')

class TrackParser {
    /**
     * 
     * @param {SMML.ParsedTrack} trackResult 
     */
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
                const dur = trackResult.Meta.Duration - content[press].StartTime
                content.splice(press, 1)
                content.slice(press).forEach((tok) => tok.Duration = dur)
                break
            }
            while (release < press) {
                content.splice(release, 1)
                release = content.findIndex((tok) => tok.Type === 'PedalRelease')
                press -= 1
            }
            const dur = content[release].StartTime - content[press].StartTime
            content.splice(release, 1)
            content.splice(press, 1)
            content.slice(press, release).forEach((tok) => tok.Duration = dur)
        }
    }

    /**
     *
     * @param {SMML.Track} track
     * @param {SMML.GlobalSetting} sectionSettings
     */
    constructor(track, sectionSettings, libraries, isSubtrack = false) {
        this.isSubtrack = isSubtrack
        this.ID = track.ID
        this.Instruments = track.Instruments
        this.Libraries = libraries
        this.Content = track.Content
        this.Settings = sectionSettings.extend()
        this.Context = {
            afterTie: false,
            notesBeforeTie: [],
            startTime: 0,
            pitchQueue: [],
            warnings: []
        }
    }

    /**
     * @returns {SMML.ParsedTrack[]}
     */
    parseTrack() {
        this.preprocess()
        const trackResult = this.parseTrackContent()
        TrackParser.processPedal(trackResult)
        if (this.isSubtrack) {
            return [trackResult]
        } else {
            return this.Instruments.map((instrument) => {
                if (instrument.Proportion === null) {
                    instrument.Proportion = 1
                }
                return {
                    Instrument: instrument.Instrument,
                    ID: this.ID ? `${this.ID}#${instrument.Instrument}` : instrument.Instrument,
                    Meta: trackResult.Meta,
                    Content: trackResult.Content.map((note) => Object.assign({}, note, { Volume: note.Volume * instrument.Proportion }))
                }
            })
        }
    }

    mergeMacro() {
        let length = this.Content.length
        let pointer = 0
        while (pointer < length) {
            const token = this.Content[pointer]
            if (token.Type === 'Macrotrack' || token.Name in this.Libraries.Track) {
                const macro = this.Libraries.Track[token.Name]
                this.Content.splice(pointer, 1, ...macro)
                // pointer += macro.length - 1
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
            last2.Terminal = true
            this.Content.push(last2)
        } else {
            if (last.Type === 'BarLine') {
                last.Terminal = true
            }
            this.Content.push(last2)
            this.Content.push(last)
        }
    }

    /**
     * parse track content
     * @returns {SMML.ParsedTrack}
     */
    parseTrackContent() {
        const result = []
        let subtrack
        let rightIncomplete
        let leftIncomplete = 0
        let leftFirst = true

        for (const token of this.Content) {
            switch (token.Type) {
            case 'FUNCTION':
            case 'Subtrack':
                if (token.Type === 'FUNCTION') {
                    subtrack = this.Libraries.FunctionPackage.applyFunction(this, token)
                    if (subtrack === undefined) continue
                } else {
                    subtrack = new SubtrackParser(token, this.Settings, this.Libraries, this.Context.pitchQueue).parseTrack()
                }
                subtrack.Content.forEach((tok) => {
                    if (tok.Type === 'Note') {
                        tok.StartTime += this.Context.startTime
                    }
                })
                // this.Context.pitchQueue.push(...subtrack.Meta.PitchQueue)
                this.Context.startTime += subtrack.Meta.Duration
                this.Context.notesBeforeTie = subtrack.Meta.NotesBeforeTie
                this.Context.warnings.push(...subtrack.Meta.Warnings.map((warning) => {
                    warning.args[0] = this.ID
                    warning.args[1].unshift(this.Content.indexOf(token))
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
                            this.Context.warnings.push(new BarLengthError(this.ID, [this.Content.indexOf(token)], rightIncomplete))
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
                        this.Context.warnings.push(new BarLengthError(this.ID, [this.Content.indexOf(token)], rightIncomplete))
                    }
                    rightIncomplete = 0
                }
                break
            case 'PedalPress':
            case 'PedalRelease':
                result.push({
                    Type: token.Type,
                    StartTime: this.Context.startTime
                })
                break
            case 'Clef':
            case 'Whitespace':
            case 'Undefined':
                break
            }
        }
        if (!this.isSubtrack && (leftIncomplete + rightIncomplete !== this.Settings.Bar)) {
            if (leftIncomplete !== this.Settings.Bar) {
                this.Context.warnings.push(new BarLengthError(this.ID, [0], leftIncomplete))
            }
            if (rightIncomplete !== this.Settings.Bar) {
                this.Context.warnings.push(new BarLengthError(this.ID, [-1], rightIncomplete))
            }
        }
        const returnObj = {
            Content: result,
            Meta: {
                Warnings: this.Context.warnings,
                PitchQueue: this.Context.pitchQueue,
                FadeIn: this.Settings.FadeIn,
                FadeOut: this.Settings.FadeOut,
                Duration: this.Context.startTime,
                Single: leftFirst,
                Incomplete: [leftIncomplete, rightIncomplete],
                NotesBeforeTie: this.Context.notesBeforeTie
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
        return bar === undefined || bar === this.Settings.Bar || bar === 0
    }

    /**
     *
     * @param {SMML.NoteToken} note
     * @returns {SMML.ParsedNote[]}
     */
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
                pitches.push(...[].concat(...queue.map((pitch) => this.Settings.Key.map((key) => key + pitch + delta))))
                volumes.push(...[].concat(...new Array(queue.length).fill(this.getVolume(note))))
            } else {
                this.Context.warnings.push(new TraceError(this.ID, [this.Content.indexOf(note)], this.Settings.Trace))
            }
        } else {
            for (const pitch of note.Pitches) {
                if (pitch.Degree === '0') continue
                if (pitch.Degree === 'x') {
                    pitches.push(null)
                    volumes.push(this.Settings.Volume[0] * note.VolOp.split('').reduce((sum, cur) => sum * cur === '>' ? this.Settings.Accent : cur === ':' ? this.Settings.Light : 1, 1))
                } else if (pitch.Chord === '') {
                    const temp = this.parsePitch(pitch, note.PitOp)
                    pitchQueue.push(temp[0])
                    pitches.push(...temp)
                    volumes.push(...this.getVolume(note))
                } else {
                    const basePitch = this.parsePitch(pitch, note.PitOp)
                    const chords = this.parseChord(pitch)
                    pitchQueue.push(...chords.map(subPitch => subPitch + basePitch[0]))
                    pitches.push(...[].concat(...chords.map((subPitch) => basePitch.map((delta) => subPitch + delta))))
                    volumes.push(...[].concat(...new Array(chords.length).fill(this.getVolume(note))))
                }
            }
        }
        if (new Set(pitches).size !== pitches.length) {
            this.Context.warnings.push(new DupChordError(this.ID, [this.Content.indexOf(note)], pitches))
        }
        if (volumes.some((volume) => volume > 1 || volume < 0)) {
            this.Context.warnings.push(new VolumeError(this.ID, [this.Content.indexOf(note)], volumes))
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
                prevNote.Duration += actualDuration
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
                StartTime: this.Context.startTime
            })
        }
        this.Context.startTime += duration
        return result
    }

    getVolume(note) {
        const scale = note.VolOp.split('').reduce((sum, cur) => sum * cur === '>' ? this.Settings.Accent : cur === ':' ? this.Settings.Light : 1, 1)
        const total = this.Settings.Key.length
        const vol = this.Settings.Volume.length
        return [...this.Settings.Volume, ...new Array(total - vol).fill(this.Settings.Volume[vol - 1])].map((v) => v * scale)
    }
    
    /**
    *
    * @param {SMML.Pitch} pitch
    * @returns {number[]}
    */
    parseChord(pitch) {
        return pitch.Chord.split('').reduce((pitches, chord) => {
            const operator = this.Libraries.Chord[chord]
            const res = []
            operator.forEach(([head, tail, delta]) => {
                if (head > 0) {
                    head -= 1
                }
                if (tail > 0) {
                    res.push(...pitches.slice(head, tail).map((pitch) => pitch + delta))
                } else if (tail === -1) {
                    res.push(...pitches.slice(head).map((pitch) => pitch + delta))
                } else {
                    res.push(...pitches.slice(head, tail + 1).map((pitch) => pitch + delta))
                }
            })
            return res
        }, [0])
    }

    /**
     *
     * @param {SMML.Pitch} pitch
     * @returns {number}
     */
    parsePitch(pitch, base) {
        const delta = this.parseDeltaPitch(base) + TrackParser.pitchDict[pitch.Degree] + this.parseDeltaPitch(pitch.PitOp)
        return this.Settings.Key.map((key) => key + delta)
    }

    parseDeltaPitch(pitchOperators) {
        return pitchOperators.split('').reduce((sum, cur) => sum + TrackParser.pitchOperatorDict[cur], 0)
    }

    /**
     *
     * @param {SMML.NoteToken} note
     * @returns {number}
     */
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
        this.Context.pitchQueue = pitchQueue.slice()
    }

    parseTrack() {
        this.preprocess()
        const trackResult = this.parseTrackContent(this.Content)
        return trackResult
    }

    preprocess() {
        this.mergeMacro()
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
        if (this.Repeat > 0) {
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
            }
            this.Content = temp
        } else {
            const skip = this.Content.findIndex((tok) => tok.Skip === true)
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
