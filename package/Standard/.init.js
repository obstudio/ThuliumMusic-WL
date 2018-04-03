module.exports = {

    // Internal Functions
    _zoom_(origin, scale, start = 0) {
        return origin.map(note => Object.assign({}, note, {
            StartTime: note.StartTime * scale + start,
            Duration: note.Duration * scale
        }));
    },

    _fill_(origin, duration, total) {
        const result = [];
        for (i = 0; i < total; i += duration) {
            result.push(...this._zoom_(origin, 1, i));
        }
        return result.filter(note => note.StartTime + note.Duration <= total);
    },

    // Standard Package
    Tremolo1(expr, subtrack) {
        /**** (^%1-)&2 ****/
        const src = this.ParseTrack(subtrack);
        const time = Math.pow(2, -expr) * 60 / this.Settings.Speed;
        const scale = time / src.Meta.Duration;
        const content = this.Library._zoom_(src.Content, scale);
        return Object.assign(src, {
            Content: this.Library._fill_(content, time, src.Meta.Duration)
        });
    },

    Tremolo2(expr, subtrack1, subtrack2) {
        /**** &2(^%1=)&3 ****/
        const src1 = this.ParseTrack(subtrack1);
        const src2 = this.ParseTrack(subtrack2);
        const time = Math.pow(2, -expr) * 60 / this.Settings.Speed;
        const scale1 = time / src1.Meta.Duration;
        const scale2 = time / src2.Meta.Duration;
        const content = [].concat(
            this.Library._zoom_(src1.Content, scale1),
            this.Library._zoom_(src2.Content, scale2, time)
        );
        return Object.assign(src2, {
            Content: this.Library._fill_(content, 2 * time, src2.Meta.Duration)
        });
    },

    Tuplet(expr, subtrack) {
        /**** (^!1~)&2 ****/
        const scale = Math.pow(2, Math.floor(Math.log2(expr))) / expr;
        const src = this.ParseTrack(subtrack, {
            Settings: { Bar: this.Settings.Bar / scale }
        });
        src.Content = this.Library._zoom_(src.Content, scale);
        src.Meta.Duration *= scale;
        src.Meta.BarFirst *= scale;
        src.Meta.BarLast *= scale;
        return src;
    },

    Portamento(subtrack1, subtrack2) {
        /**** &1~&2 ****/
        const src1 = this.ParseTrack(subtrack1);
        const src2 = this.ParseTrack(subtrack2);
        const note1 = src1.Content.pop();
        const note2 = src2.Content[0];
        const duration = src1.Meta.Duration - note1.StartTime;
        const port = this.Settings.getOrSetDefault('Port', 6);
        const num = duration * port * this.Settings.Speed / 60;
        const pitchStep = (note2.Pitch - note1.Pitch) / (num - 1);
        const volumeStep = (note2.Volume - note1.Volume) / (num - 1);

        if (Math.abs(pitchStep) < 1) {
            this.ReportError('Undersize', {
                StartPitch: note1.Pitch,
                EndPitch: note2.Pitch,
                PitchCount: num
            });
        }

        for (let i = 0; i < num; i++) {
            src1.Content.push({
                Type: 'Note',
                Pitch: Math.round(note1.Pitch + pitchStep * i),
                Volume: note1.Volume + volumeStep * i,
                Duration: 60 / this.Settings.Speed / port,
                StartTime: 60 / this.Settings.Speed / port * i
            });
        }

        return this.JoinTrack(src1, src2);
    },

    GraceNote(subtrack1, subtrack2) {
        /**** (^&1\^)&2 ****/
        const src1 = this.ParseTrack(subtrack1);
        const src2 = this.ParseTrack(subtrack2);
        const seg = this.Settings.getOrSetDefault('Seg', 1 / 4)
        const scale = seg / Math.max(src1.Content.length, 4);
        const duration = src1.Meta.Duration * scale;
        const content = this.Library._zoom_(src1.Content, scale);

        src2.Content.forEach(note => {
            if (note.StartTime < duration) {
                if (note.Duration + note.StartTime > duration) {
                    content.push(Object.assign({}, note, {
                        StartTime: duration,
                        Duration: note.Duration + note.StartTime - duration
                    }));
                } else {
                    this.ReportError('Overlong', {
                        Duration: duration,
                        Encounter: note
                    });
                }
            } else {
                content.push(note);
            }
        });

        return Object.assign(src2, { Content: content });
    },

    Appoggiatura(subtrack1, subtrack2) {
        /**** &1(\^^&2) ****/
        const src1 = this.ParseTrack(subtrack1);
        const src2 = this.ParseTrack(subtrack2);
        const seg = this.Settings.getOrSetDefault('Seg', 1 / 4)
        const scale = seg / Math.max(src2.Content.length, 4);
        const start = src1.Meta.Duration - src2.Meta.Duration * scale;
        const content = this.Library._zoom_(src2.Content, scale, start);

        src1.Content.forEach(note => {
            if (note.StartTime + note.Duration > start) {
                if (note.StartTime < start) {
                    content.push(Object.assign({}, note, {
                        Duration: start - note.StartTime
                    }));
                } else {
                    this.ReportError('Overlong', {
                        Duration: duration,
                        Encounter: note
                    });
                }
            } else {
                content.push(note);
            }
        });

        return Object.assign(src1, { Content: content });
    },

    Fermata(subtrack) {
        /**** (.)&1 ****/
        const src = this.ParseTrack(subtrack);
        const ferm = this.Settings.getOrSetDefault('Ferm', 2);
        src.Content = this.Library._zoom_(src.Content, ferm);
        src.Meta.Duration *= ferm;
        return src;
    },

    Arpeggio(subtrack) {
        /**** \$&1 ****/
        const src = this.ParseTrack(subtrack);
        const seg = this.Settings.getOrSetDefault('Seg', 1 / 4)
        const scale = seg / Math.max(src.Content.length, 4);
        src.Content.forEach((note, index) => {
            if (note.StartTime == 0) {
                note.StartTime = scale * index;
                note.Duration -= scale * index;
            }
        });
        src.Content.unshift({ Type: 'PedalPress', StartTime: 0 });
        src.Content.push({ Type: 'PedalRelease', StartTime: src.Meta.Duration });
        return src;
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
        this.Settings.assignSetting('Speed', speed, (speed) => speed > 0);
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
