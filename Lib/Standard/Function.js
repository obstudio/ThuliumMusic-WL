module.exports = {
    Vol(volume) {
        AssignSetting(this, 'Vol', volume, Criteria.Vol)
    },
    Spd(speed) {
        AssignSetting(this, 'Speed', speed, Criteria.Spd)
    },
    Key(key) {
        AssignSetting(this, 'Key', key, Criteria.Key)
    },
    Oct(oct) {
        AssignSetting(this, 'Octave', oct, Criteria.Oct)
    },
    KeyOct(key, oct) {
        AssignSetting(this, 'Key', key, Criteria.Key)
        AssignSetting(this, 'Octave', oct, Criteria.Oct)
    },
    Beat(beat) {
        AssignSetting(this, 'Beat', beat, Criteria.Beat)
    },
    Bar(bar) {
        AssignSetting(this, 'Bar', bar, Criteria.Bar)
    },
    BeatBar(beat, bar) {
        AssignSetting(this, 'Beat', beat, Criteria.Beat)
        AssignSetting(this, 'Bar', bar, Criteria.Bar)
    },
    Dur(scale) {
        AssignSetting(this, 'Duration', scale, Criteria.Dur)
    },
    Stac1(restProportion) {
        AssignSetting(this, 'Stac1', restProportion, Criteria.Stac1)
    },
    Stac2(restProportion) {
        AssignSetting(this, 'Stac2', restProportion, Criteria.Stac2)
    },
    Acct(scale) {
        AssignSetting(this, 'Accent', scale, Criteria.Acct)
    },
    Light(scale) {
        AssignSetting(this, 'Light', scale, Criteria.Light)
    },
    Appo(r) {
        AssignSetting(this, 'Appo', r, Criteria.Appo)
    },
    Port(r) {
        AssignSetting(this, 'Port', r, Criteria.Port)
    },
    Trace(count) {
        AssignSetting(this, 'Trace', count, Criteria.Trace)
    },
    FadeIn(time) {
        AssignSetting(this, 'FadeIn', time, Criteria.FadeIn)
    },
    FadeOut(time) {
        AssignSetting(this, 'FadeOut', time, Criteria.FadeOut)
    },
    Rev(r) {
        AssignSetting(this, 'Rev', r, Criteria.Rev)
    },
    Var(index, obj) {
        this.Var[index] = obj
    }
}

const Criteria = {
    Vol:     (volume) => volume <= 1 && volume >= 0,
    Spd:     (speed) => speed > 0 && Number.isInteger(speed),
    Key:     (key) => Number.isInteger(key),
    Oct:     (octave) => Number.isInteger(octave),
    Beat:    (beat) => beat > 0 && Number.isInteger(beat),
    Bar:     (bar) => bar > 0 && Number.isInteger(Math.log2(bar)),
    Dur:     (scale) => scale > 0,
    Stac1:   (restProportion) => restProportion >= 0 && restProportion <= 0,
    Stac2:   (restProportion) => restProportion >= 0 && restProportion <= 0,
    Acct:    (scale) => scale > 1,
    Light:   (scale) => scale < 1 && scale > 0,
    Appo:    (r) => r > 0,
    Port:    (r) => r > 0,
    Trace:   (count) => count > 0 && Number.isInteger(count),
    FadeIn:  (time) => time > 0,
    FadeOut: (time) => time > 0,
    Rev:     () => true,
}

/**
 * 
 * @param {SMML.GlobalSetting} globalSetting 
 * @param {string} key 
 * @param {number} value 
 * @param {function} criterion 
 */
function AssignSetting(globalSetting, key, value, criterion) {
    if (typeof value !== 'number') throw new TypeError(`Non-numeric value passed in as ${key}`)
    if (!criterion(value)) throw new RangeError(`${key} out of range`)
    globalSetting[key] = value
}

// consider using Proxy
/* new Proxy({}, {
    get (target, key) {}
}) */
