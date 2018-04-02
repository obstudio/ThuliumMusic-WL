const { SubtrackParser } = require('../../library/Parser/TrackParser')

module.exports = {
    
    _adjust_(src, beats) {
        if (this.Meta.BarCount === 0) {
            beats += this.Meta.BarFirst;
            src.Meta.BarCount = Math.floor(beats / this.Settings.Bar);
            src.Meta.BarLast = beats % this.Settings.Bar;
            src.Meta.BarFirst = this.Settings.Bar - this.Meta.BarFirst;
        } else {

        }
        return src;
    },

    Fill(subtrack, beats) {
        const src = ParseTrack(subtrack);
        const time = beats * 60 / this.Settings.Speed;
        const content = this.Library._fill_(src.Content, src.Meta.Duration, time);

        return this.Library._push_(Object.assign(src, ))
    },

    OctShift(delta) {
        for (var i = 0, length = this.Settings.Key.length; i < length; i++) {
            this.Settings.Key[i] += delta * 12
        }
    },
    
    SpdShift(delta) {
        this.Settings.Speed *= delta
    },
    
    Bar(bar) {
        this.Settings.assignSetting('Bar', bar, (bar) => bar > 0 && Number.isInteger(bar))
    },

    Beat(beat) {
        this.Settings.assignSetting('Beat', beat, (beat) => beat > 0 && Number.isInteger(Math.log2(beat)))
    },

    Drop(subtrack, beats) {
        const src = ParseTrack(subtrack);
        const result = [];
        let time;

        if (beats > 0) {
            time = beats * 60 / this.Settings.Speed;
            src.Content.forEach(note => {
                if (note.StartTime >= time) {
                    result.push(Object.assign({}, note, { StartTime: note.StartTime - time }));
                }
            });
        } else if (beat < 0) {
            time = -beats * 60 / src.Settings.Speed;
            src.Content.forEach(note => {
                if (note.StartTime < src.Meta.Duration - time) {
                    result.push(Object.assign({}, note));
                }
            });
        } else {
            result.push(...src.Content);
        }

        if (time >= src.Meta.Duration) {
            this.ReportError('TooShort', { Required: time, Actual: arc.Meta.Duration });
        }

        return result;
    },

    Prod(template, track) {
        const t = new SubtrackParser(subtrack, this.Settings, this.Libraries, this.pitchQueue).parseTrack()
    }

}

