const { SubtrackParser } = require('../../library/Parser/TrackParser')

module.exports = {
    
    KeyShift(delta) {
        for (var i = 0, length = this.Settings.Key.length; i < length; i++) {
            this.Settings.Key[i] += delta
        }
    },
    
    OctShift(delta) {
        for (var i = 0, length = this.Settings.Key.length; i < length; i++) {
            this.Settings.Key[i] += delta * 12
        }
    },
    
    SpdShift(delta) {
        this.Settings.Speed *= delta
    },
    
    Drop(beats, subtrack) {
        const t = new SubtrackParser(subtrack, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
    },

    Fill(beats, subtrack) {
        const t = new SubtrackParser(subtrack, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
    }

    Rest(beats) {
        return Fill(beats, 0)
    }
}
