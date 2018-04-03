module.exports = {
    
    // Internal Functions
    _take_(origin, start, end) {
        origin = this._zoom_(origin.filter(note => {
            note.StartTime >= start && note.StartTime + note.Duration < end
        }), 1, -start);
    },

    _adjust_(matrix, source) {
        const mLast = matrix.Meta.BarLast;
        const mBar = matrix.Settings.bar;
        const sLast = source.Meta.BarLast;
        const sBar = source.Settings.bar;
        if (mLast >= mBar) return source;
        if (source.Meta.BarCount === 0) {
            source.Meta.BarCount = Math.floor((mLast + sLast) / sBar);
            if (source.Meta.BarCount > 0) {
                source.Meta.BarFirst = mBar - mLast;
                source.Meta.BarLast = (mLast + sLast) % sBar;
            }
        } else {
            source.Meta.BarCount += Math.floor((mLast + sLast) / sBar);
            source.Meta.BarFirst = (mLast + source.Meta.BarFirst) % mBar;
            source.Meta.BarLast = (mLast + sLast) % sBar;
        }
        return source;
    },

    // Benzene Package
    Take(subtrack, beats) {
        const src = this.ParseTrack(subtrack);
        if (beats > 0) {
            this.Library._take_(src.Content, 0, beats * 60 / this.Settings.Speed);
            if (src.Meta.BarCount === 0) {
                src.Meta.BarFirst -= beats;
            } else {
                src.Meta.BarCount -= Math.ceil((beats - src.Meta.BarFirst) / this.Settings.Bar);
                src.Meta.BarFirst = (beats - src.Meta.BarFirst) % this.Settings.Bar;
            }
        } else if (beats < 0) {
            this.Library._take_(src.Content, beats * 60 / src.Settings.Speed, src.Meta.Duration);
            if (src.Meta.BarCount === 0) {
                src.Meta.BarFirst -= beats;
            } else {
                src.Meta.BarCount -= Math.ceil((beats - src.Meta.BarLast) / src.Settings.Bar);
                src.Meta.BarLast = (beats - src.Meta.BarLast) % src.Settings.Bar;
                if (src.Meta.BarCount === 0) src.Meta.BarFirst += src.Meta.BarLast - src.Settings.Bar;
            }
        } else {
            this.ReportError('Arg::ZeroSpec', {});
        }
        return src;
    },

    Fill(subtrack, bars) {
        const src = this.ParseTrack(subtrack);
        const time = bars * this.Settings.Bar * 60 / this.Settings.Speed;
        const content = this.Library._fill_(src.Content, src.Meta.Duration, time);
        return this.Library._adjust_(this, {
            Content: content,
            Meta: Object.assign(src.Meta, {
                BarCount: bars,
                BarFirst: this.Settings.Bar,
                BarLast: 0
            }),
            Settings: src.Settings
        });
    },

    Map(template, subtrack) {
        /**** &1\@&2 ****/
        const temp = this.ParseTrack(template);
        const src = this.ParseTrack(subtrack);
        const content = [];
        src.Content.forEach(sNote => {
            const sStart = sNote.StartTime * temp.Meta.Duration;
            temp.Content.forEach(tNote => {
                const tStart = tNote.StartTime * sNote.Duration;
                const tempo = 
                content.push({
                    Type: 'Note',
                    Pitch: sNote.Pitch + tNote.Pitch,
                    Volume: sNote.Volume * tNote.Volume,
                    Duration: sNote.Duration * tNote.Duration,
                    StartTime: sStart + tStart
                });
            });
        });
        return this.Library._adjust_(this, {
            Content: content,
            Meta: Object.assign(src.Meta, {
                BarCount: Math.ceil()
            }),
            Settings: src.Settings
        });
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
    }

}

