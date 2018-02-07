module.exports = {
    Tremolo1 () {

    },

    Tremolo2 (expr, subtrack) {

    },

    Portamento (subtrack1, subtrack2) {

    },

    Appoggiatura (subtrack) {

    },

    Vol(volume) {
        AssignSetting(this, 'Volume', volume / 100, Criteria.Vol)
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
        AssignSetting(this, 'Key', Tonality[key], Criteria.Key)
        AssignSetting(this, 'Octave', oct, Criteria.Oct)
    },
    Beat(beat) {
        AssignSetting(this, 'Beat', beat, Criteria.Beat)
    },
    Bar(bar) {
        AssignSetting(this, 'Bar', bar, Criteria.Bar)
    },
    BarBeat(bar, beat) {
        AssignSetting(this, 'Bar', bar, Criteria.Bar)
        AssignSetting(this, 'Beat', beat, Criteria.Beat)
    },
    Dur(scale) {
        AssignSetting(this, 'Duration', scale, Criteria.Dur)
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
    setVar(key, value) {
        this.Var[key] = value
    },
    getVar(key, defaultValue = null) {
        return this.Var[key] ? this.Var[key] : defaultValue
    },
    Stac(restProportion, index = 1) {
        if (typeof restProportion !== 'number') throw new TypeError('Non-numeric value passed in as Stac')
        if (!Criteria.Stac(restProportion)) throw new RangeError('Stac out of range')
        if (![0, 1, 2].indexOf(index)) throw new RangeError('Stac index out of range')
        this.Stac[index] = restProportion
    },
}

const Criteria = {
    Vol:     (volume) => volume <= 1 && volume >= 0,
    Spd:     (speed) => speed > 0,
    Key:     (key) => Number.isInteger(key),
    Oct:     (octave) => Number.isInteger(octave),
    Beat:    (beat) => beat > 0 && Number.isInteger(Math.log2(beat)),
    Bar:     (bar) => bar > 0 && Number.isInteger(bar),
    Dur:     (scale) => scale > 0,
    Stac:    (restProportion) => restProportion >= 0 && restProportion <= 1,
    Acct:    (scale) => scale > 1,
    Light:   (scale) => scale < 1 && scale > 0,
    Appo:    (r) => r > 0,
    Port:    (r) => r > 0,
    Trace:   (count) => count > 0 && count <= 4 && Number.isInteger(count),
    FadeIn:  (time) => time > 0,
    FadeOut: (time) => time > 0,
    Rev:     () => true,
}
const Tonality = {
    'C':    0,
    'G':    7,
    'D':    2,
    'A':    9,
    'E':    4,
    'B':    -1,
    '#F':   6,
    '#C':   1,
    'F':    5,
    'bB':   -2,
    'bE':   3,
    'bA':   8,
    'bD':   1,
    'bG':   6,
    'bC':   -1,

    'F#':   6,
    'C#':   1,
    'Bb':   -2,
    'Eb':   3,
    'Ab':   8,
    'Db':   1,
    'Gb':   6,
    'Cb':   -1,
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
