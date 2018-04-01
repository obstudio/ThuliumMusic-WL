class GlobalSetting {
    constructor({
        Key = [0],
        Volume = [1],
        Bar = 4,
        Beat = 4,
        Speed = 60,
        Stac = [0, 1 / 2, 3 / 4],
        Accent = 2,
        Light = 1 / 2,
        Trace = 1,
        Duration = 0,
        FadeIn = 0,
        FadeOut = 0,
        Rev = 0
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
        this.FadeIn = FadeIn
        this.FadeOut = FadeOut
        this.Rev = Rev
    }

    getOrSetDefault(key, defaultValue) {
        if (key in this) {
            return this[key]
        } else {
            if (defaultValue) this[key] = defaultValue
            return defaultValue
        }
    }

    extend(settingObj = { Stac: this.Stac.slice(), Key: this.Key.slice(), Volume: this.Volume.slice() }) {
        const newSetting = new GlobalSetting()
        Object.assign(newSetting, this, settingObj)
        return newSetting
    }

    update(settingObj) {
        Object.assign(this, settingObj)
    }

    /**
     *
     * @param {Tm.GlobalSetting} globalSetting
     * @param {string} key
     * @param {number} value
     * @param {function} criterion
     */
    assignSetting(key, value, criterion) {
        if (this[key] instanceof Array) {
            if (value instanceof Array) {
                if (!value.every((v) => typeof value === 'number')) {
                    throw new TypeError(`Non-numeric value passed in as ${key} element`)
                }
                if (!value.every((v) => criterion(v))) {
                    throw new RangeError(`${key} out of range`)
                }
                this[key] = value
            } else {
                throw new TypeError(`Non-array value passed in as ${key}`)
            }
        } else {
            if (typeof value !== 'number') throw new TypeError(`Non-numeric value passed in as ${key}`)
            if (!criterion(value)) throw new RangeError(`${key} out of range`)
            this[key] = value
        }
    }

    assignSettingAtIndex(key, index, value, criterion) {
        if (this[key] instanceof Array) {
            if (typeof value !== 'number') throw new TypeError(`Non-numeric value passed in as ${key}`)
            if (typeof index !== 'number' || index < 0 || !Number.isInteger(index)) throw new TypeError(`Non-numeric index passed in as ${key} index`)
            if (!criterion(value)) throw new RangeError(`${key} out of range`)
            this[key][index] = value
        } else {
            throw new TypeError(`Non-array value passed in as ${key}`)
        }
    }
}

module.exports = GlobalSetting
