class GlobalSetting {
    constructor({
        Key = 0,
        Bar = 4,
        Beat = 4,
        Speed = 60,
        Volume = 1.0,
        Stac = [0, 1 / 2, 3 / 4],
        Accent = 1,
        Light = 1,
        Trace = 1,
        Duration = 0,
        Oct = 0,
        FadeIn = 0,
        FadeOut = 0,
        Rev = 0,
        ConOct = 0,
        ConOctVolume = 1,
    } = {}) {
        this.Key = Key
        this.Bar = Bar
        this.Beat = Beat
        this.Speed = Speed
        this.Volume = Volume
        this.Stac = Stac
        this.Accent = Accent
        this.Light = Light
        this.Trace = Trace
        this.Duration = Duration
        this.Octave = Oct
        this.FadeIn = FadeIn
        this.FadeOut = FadeOut
        this.Rev = Rev
        this.ConOct = ConOct
        this.ConOctVolume = ConOctVolume
        this.Var = {}
    }

    getOrSetDefault(key, defaultValue) {
        if (key in this) {
            return this[key]
        } else {
            if (defaultValue) this[key] = defaultValue
            return defaultValue
        }
    }

    extend(settingObj = {Stac: this.Stac.slice()}) {
        const newSetting = new GlobalSetting()
        Object.assign(newSetting, this, settingObj)
        return newSetting
    }

    update(settingObj) {
        Object.assign(this, settingObj)
    }

    /**
     * 
     * @param {SMML.GlobalSetting} globalSetting 
     * @param {string} key 
     * @param {number} value 
     * @param {function} criterion 
     */
    assignSetting(key, value, criterion) {
        if (typeof value !== 'number') throw new TypeError(`Non-numeric value passed in as ${key}`)
        if (!criterion(value)) throw new RangeError(`${key} out of range`)
        this[key] = value
    }

}

module.exports = GlobalSetting
