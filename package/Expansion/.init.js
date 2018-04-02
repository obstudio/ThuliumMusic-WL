const { SubtrackParser } = require('../../library/Parser/TrackParser')

module.exports = {
    
    OctShift(delta) {
        for (var i = 0, length = this.Settings.Key.length; i < length; i++) {
            this.Settings.Key[i] += delta * 12
        }
    },
    
    SpdShift(delta) {
        this.Settings.Speed *= delta
    },
    
    _drop_(data, time) {
        const ectype = data
        if (time > 0) {
            data.Content = data.Content.filter((note) => note.StartTime < time)
            ectype.Meta.duration -= time
        } else if (time < 0) {
            ectype.Content = data.Content.filter((note) => note.StartTime >= data.Meta.duration + time)
            ectype.Meta.duration += time
        }
        return ectype
    },

    Drop(subtrack, beats) {
        const data = ParseTrack(subtrack, "default")
        if (data.Meta.beatCount < Math.abs(beats)) {
            // error
        } else {
            return this._drop_(data, beats * 60 / this.Settings.Speed)
        }
    },

    Fill(subtrack, beats) {
        const result = []
        const data = ParseTrack(subtrack, "single")
        //if Math.isInteger()
        this.Settings.Beat
        return {
            Content: result,
            Settings: this.Settings,
            Meta: Object.defineProperties(data.meta, {
            })
        }
    },

    Prod(template, track) {
        const t = new SubtrackParser(subtrack, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
    }

}

