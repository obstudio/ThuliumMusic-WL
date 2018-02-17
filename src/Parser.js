const Loader = require('./LibLoader')
const GlobalSetting = require('./GlobalSetting')
const { TrackParser } = require('./TrackParser')

class Parser {
    /**
     * SMML Parser
     * @param {SMML.TokenizedData} tokenizedData 经过tok的JSON对象
     * @param {SMML.Adapter} adapter 可选的Adapter
     * @example
     * new Parser(tokenizedData)
     * new Parser(tokenizedData, MIDIAdapter)
     * new Parser(tokenizedData, MMAAdapter)
     */
    constructor(tokenizedData, adapter = undefined) {
        this.tokenizedData = tokenizedData
        this.libraries = new Loader(this.tokenizedData.Library).load()
        this.result = {
            Sections: undefined
        }
        this.sectionContext = {
            Settings: new GlobalSetting()
        }
        this.order = []
        this.adapter = adapter
    }

    parse() {
        const result = []
        this.generateOrder()
        this.order.forEach((index) => {
            result.push(this.parseSection(this.tokenizedData.Sections[index]))
        })
        this.result = result
        if (this.adapter === undefined) {
            return result
        } else {
            return new this.adapter(result).adapt()
        }
    }

    generateOrder() {
        const temp = this.tokenizedData.Sections.map((section) => {
            const vol = section.Settings.find((setting) => setting.Type === 'Volta')
            let volta
            if (vol) {
                volta = vol.Order
            } else {
                volta = undefined
            }
            return {
                RepeatBegin: section.Settings.find((setting) => setting.Type === 'RepeatBegin') ? true : false,
                RepeatEnd: section.Settings.find((setting) => setting.Type === 'RepeatEnd') ? true : false,
                Volta: volta
            }
        })
        const length = temp.length
        let pointer = 0
        let repeatBeginIndex = 0
        let order = 1
        let skip = true
        while (pointer < length) {
            if (temp[pointer].RepeatEnd) {
                if (skip) temp[pointer].RepeatEnd = false
                if (order === 1) {
                    pointer = repeatBeginIndex
                    order += 1
                    continue
                } else {
                    let pointer1 = repeatBeginIndex
                    let flag = false
                    while (pointer1 < length) {
                        if (temp[pointer1].RepeatBegin) break
                        if (temp[pointer1].Volta && temp[pointer1].Volta.indexOf(order + 1) !== -1) {
                            flag = true
                            break
                        }
                        pointer1 += 1
                    }
                    if (flag) {
                        pointer = repeatBeginIndex
                        order += 1
                        continue
                    } else {
                        this.order.push(pointer)
                        pointer += 1
                    }
                }
            }
            if (temp[pointer].RepeatBegin) {
                repeatBeginIndex = pointer
                temp[pointer].RepeatBegin = false
                order = 1
            } else if (temp[pointer].Volta) {
                if (temp[pointer].Volta.indexOf(order) === -1) {
                    pointer += 1
                } else {
                    temp[pointer].Volta.splice(temp[pointer].Volta.indexOf(order), 1)
                    skip = (temp[pointer].Volta.length === 0)
                    this.order.push(pointer)
                    pointer += 1
                }
            } else {
                this.order.push(pointer)
                pointer += 1
            }
        }
    }

    /**
     * parse section
     * @param {SMML.Section} section
     */
    parseSection(section) {
        section.Settings.filter((token) => token.Type === 'FUNCTION')
            .forEach((token) => this.libraries.FunctionPackage.applyFunction({ Settings: this.sectionContext.Settings }, token))
        const instrStatistic = {}
        return {
            ID: section.ID,
            Tracks: [].concat(...section.Tracks.map((track) => {
                const tempTracks = new TrackParser(track, this.sectionContext.Settings, this.libraries).parseTrack()
                for (const tempTrack of tempTracks) {
                    if (tempTrack.Instrument in instrStatistic) {
                        instrStatistic[tempTrack.Instrument] += 1
                    } else {
                        instrStatistic[tempTrack.Instrument] = 1
                    }
                    if (track.ID === '') {
                        tempTrack.ID += '#' + instrStatistic[tempTrack.Instrument].toString()
                    }
                }
                return tempTracks
            }))
        }
    }
}

module.exports = Parser
