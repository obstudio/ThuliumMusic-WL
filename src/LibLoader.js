// eslint-disable-next-line no-unused-vars
const { SubtrackParser } = require('./TrackParser')

class LibLoader {
    /**
     *
     * @param {SMML.Library[]} libs
     */
    constructor(libs = [], withDefault = true) {
        this.libs = libs

        this.result = {
            Chord: {},
            MetaInformation: {},
            FunctionPackage: {
                Custom: {}
            },
            MIDIEventList: {},
            Track: {}
        }
        if (withDefault) {
            Object.assign(this.result, LibLoader.Default)
        }
    }

    load() {
        for (const lib of this.libs) {
            this.loadLibrary(lib)
        }
        return this.result
    }

    /**
     * load internal lib
     * @param {SMML.InternalLibrary} lib 
     */
    loadLibrary(lib) {
        switch (lib.Type) {
        case LibLoader.libType.Chord:
            lib.Data.forEach((operator) => {
                this.result.Chord[operator.Notation] = operator.Pitches
            })
            break
        case LibLoader.libType.Track:
            for (const track of lib.Data) {
                this.result.Track[track.Name] = track.Content
            }
            break
        case LibLoader.libType.MetaInformation:
            break
        case LibLoader.libType.FunctionPackage:
            this.loadCode(lib.Data)
            break
        case LibLoader.libType.MIDIEventList:
            break
        case LibLoader.libType.Library:
            this.loadSubPackage(lib.Content)
        }
    }

    loadCode(data) {
        const code = 'this.result.FunctionPackage.Custom = {' + data.map((func) => func.Code).join(',') + '}'
        eval(code)
    }

    /**
     * 
     * @param {SMML.Library[]} content 
     */
    loadSubPackage(content) {
        const sub = new LibLoader(content, false).load()
        Object.assign(this.result.Chord, sub.Chord)
        Object.assign(this.result.FunctionPackage.Custom, sub.FunctionPackage.Custom)
        Object.assign(this.result.MetaInformation, sub.MetaInformation)
        Object.assign(this.result.MIDIEventList, sub.MIDIEventList)
        Object.assign(this.result.Track, sub.Track)
    }
}

LibLoader.libType = {
    Chord: 'Chord',
    MetaInformation: 'MetaInformation',
    FunctionPackage: 'Function',
    MIDIEventList: 'MIDIEventList',
    Library: 'Package',
    Track: 'Track'
}

LibLoader.Default = {
    Chord: {
        o: [[0, -1, 0], [1, 1, 12]],
        u: [[-1, -1, -12], [0, -1, 0]],
        i: [[1, 1, 12], [2, -1, 0]],
        j: [[1, 2, 12], [3, -1, 0]]
    },
    MetaInformation: {},
    FunctionPackage: {
        STD: require('./STD'),
        Custom: {},
        applyFunction(parser, token) {
            return this.locateFunction(token.Name).apply({
                Settings: parser.Settings,
                Libraries: parser.Libraries,
                pitchQueue: parser.Context.pitchQueue
            }, token.Argument.map((arg) => {
                switch (arg.Type) {
                case 'Read':
                    return Number(arg.Content)
                case 'String':
                    return arg.Content
                case 'Expression':
                    return eval(arg.Content.replace(/Log2/g, 'Math.log2'))
                default:
                    return arg
                }
            }))
        },
        locateFunction (name) {
            if (name in this.STD) return this.STD[name]
            if (name in this.Custom) return this.Custom[name]   // can be changed to employ package mechanism
            return () => {}
        }
    },
    MIDIEventList: {},
    Track: {}
}

module.exports = LibLoader
