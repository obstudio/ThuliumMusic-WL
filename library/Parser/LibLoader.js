const { SubtrackParser } = require('./TrackParser')
const { parse } = require('acorn')

const packagePath = '../../package/'
const packageInfo = require(packagePath + 'index.json')

class LibLoader {
    /**
     *
     * @param {Tm.Library[]} libs
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
     * @param {Tm.InternalLibrary} lib
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
        const result = parse(data)
        if (!result.body.every((stmt) => stmt.type === 'FunctionDeclaration')) return
        const code = data + '\nreturn {' + result.body.map((stmt) => stmt.id.name).join(',') + '}'
        try {
            /* eslint-disable-next-line no-new-func */
            Object.assign(this.result.FunctionPackage.Custom, new Function(code)()) // FIXME: change to other methods
        } catch (e) {
            console.log('Script grammar error')
        }
    }

    /**
     *
     * @param {Tm.Library[]} content
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
        M: [[1, 1, 0], [1, 1, 4], [1, 1, 7]],
        m: [[1, 1, 0], [1, 1, 3], [1, 1, 7]],
        a: [[1, 1, 0], [1, 1, 4], [1, 1, 8]],
        d: [[1, 1, 0], [1, 1, 3], [1, 1, 6]],
        t: [[1, -1, 0], [1, 1, 3]],
        T: [[1, -1, 0], [1, 1, 4]],
        q: [[1, -1, 0], [1, 1, 5]],
        Q: [[1, -1, 0], [1, 1, 6]],
        p: [[1, -1, 0], [1, 1, 7]],
        P: [[1, -1, 0], [1, 1, 8]],
        h: [[1, -1, 0], [1, 1, 9]],
        H: [[1, -1, 0], [1, 1, 10]],
        s: [[1, -1, 0], [1, 1, 11]],
        o: [[1, -1, 0], [1, 1, 12]],
        u: [[-1, -1, -12], [1, -1, 0]],
        i: [[1, 1, 12], [2, -1, 0]],
        j: [[1, 2, 12], [3, -1, 0]],
        k: [[1, 3, 12], [4, -1, 0]]
    },
    MetaInformation: {},
    FunctionPackage: {
        STD: require(packagePath + packageInfo.AutoLoad + '/.init'),
        Custom: {},
        applyFunction(parser, token) {
            return this.locateFunction(token.Name).apply({
                ParseTrack(track, {
                    protocol = 'Default',
                    settings = null
                } = {}) {
                    return new SubtrackParser(
                        track,
                        settings === null ? parser.Settings : parser.Settings.extend(settings),
                        parser.Libraries,
                        wrap(parser.Meta, protocol)
                    ).parseTrack();
                },
                ReportError(name, args) {
                    if (!name.includes('::')) {
                        name = 'Func::' + token.Name + '::' + name;
                    }
                    parser.pushError(name, args);
                },
                JoinTrack(src1, ...rest) {
                    const result = {
                        Meta: Object.assign(src1.Meta),
                        Content: src1.Content.slice(),
                        Warnings: src1.Warnings.slice(),
                        Settings: parser.Settings,
                        pushError: parser.pushError,
                        isLegalBar: parser.isLegalBar
                    };
                    for (let src of rest) {
                        result.Content.push(...src.Content.map(note => {
                            return Object.assign({}, note, {
                                StartTime: note.StartTime + result.Meta.Duration
                            });
                        }));
                        parser.mergeMeta(result, src);
                    };
                    return result;
                },
                Library: this.implicitLibCall,
                Settings: parser.Settings,
                Meta: parser.Meta,
                EmptyTrack: {
                    Type: 'Subtrack',
                    Content: [],
                    Repeat: -1
                }
            }, token.Argument.map((arg) => {
                switch (arg.Type) {
                    case 'Number':
                    case 'String':
                        return arg.Content
                    case 'Expression':
                        /* eslint-disable-next-line no-eval */
                        return eval(arg.Content.replace(/Log2/g, 'Math.log2'))
                    default:
                        return arg
                }
            }))
        },
        get implicitLibCall() {
            delete this.implicitLibCall
            this.implicitLibCall = new Proxy({}, {
                get: (_, name) => this.locateFunction(name)
            })
            return this.implicitLibCall
        },
        locateFunction(name) {
            if (name in this.STD) return this.STD[name]
            if (name in this.Custom) return this.Custom[name]
            return () => {}
        }
    },
    MIDIEventList: {},
    Track: {}
}

const Protocols = {
    Default: {
        Read: ['PitchQueue'],
        Write: ['PitchQueue']
    }
}

function wrap(meta, protocol) {
    const protocolList = Protocols[protocol]
    return new Proxy(meta, {
        get(obj, prop) {
            if (protocolList.Read.includes(prop)) {
                return obj[prop]
            }
            return null
        },
        set(obj, prop, val) {
            if (protocolList.Write.includes(prop)) {
                obj[prop] = val
            }
        }
    })
}

module.exports = LibLoader
