const { SubtrackParser } = require('./TrackParser')

const packagePath = '../../package/'
const packageInfo = require(packagePath + 'index.json')

class tmLoader {
    /**
     * Tm Library Loader
     * @param {Tm.Syntax} Thulium Syntax Object
     */
    constructor(syntax) {
        this.Chord = tmLoader.loadChord(syntax.Chord);
        this.Package = new tmPackage(syntax.Code, syntax.Dict);
        this.Track = {};
    }

    static loadChord(dict) {
        const result = {};
        dict.forEach(chord => {
            result[chord.Notation] = chord.Pitches;
        });
        return result;
    }
}

class tmPackage {
    constructor(source, dict) {
        console.log(dict);
        this.Dict = new Function(`${source}
            return {${dict.map(func => func.Name).join(',')}};
        `)();
    }

    applyFunction(parser, token) {
        const API = new tmAPI(parser, token, this.Dict);
        return this.Dict[token.Name].apply(API, tmPackage.getArguments(token.Args));
    }

    static getArguments(args) {
        return args.map(arg => {
            switch (arg.Type) {
                case 'Number':
                case 'String':
                    return arg.Content;
                case 'Expression':
                    /* eslint-disable-next-line no-eval */
                    return eval(arg.Content.replace(/Log2/g, 'Math.log2'));
                default:
                    return arg;
            }
        });
    }
}

const Protocols = {
    Default: {
        Read: ['PitchQueue'],
        Write: ['PitchQueue']
    }
}

class tmAPI {
    constructor(parser, token, dict) {
        Object.assign(this, parser);
        this.Token = token;
        this.EmptyTrack = {
            Type: 'Subtrack',
            Content: []
        };
        this.Library = new Proxy({}, {
            get: (_, name) => dict[name]
        });
    }

    ParseTrack(track, { protocol = 'Default', settings = null } = {}) {
        return new SubtrackParser(
            track,
            settings === null ? this.Settings : this.Settings.extend(settings),
            this.Libraries,
            tmAPI.wrap(this.Meta, protocol)
        ).parseTrack();
    }

    ReportError(name, args) {
        if (!name.includes('::')) {
            name = 'Func::' + this.Token.Name + '::' + name;
        }
        this.pushError(name, args);
    }

    JoinTrack(src1, ...rest) {
        const result = {
            Meta: Object.assign(src1.Meta),
            Content: src1.Content.slice(),
            Warnings: src1.Warnings.slice(),
            Settings: this.Settings,
            pushError: this.pushError,
            isLegalBar: this.isLegalBar
        };
        for (let src of rest) {
            result.Content.push(...src.Content.map(note => {
                return Object.assign({}, note, {
                    StartTime: note.StartTime + result.Meta.Duration
                });
            }));
            this.mergeMeta(result, src);
        };
        return result;
    }

    static wrap(meta, protocol) {
        const protocolList = Protocols[protocol];
        return new Proxy(meta, {
            get(obj, prop) {
                if (protocolList.Read.includes(prop)) {
                    return obj[prop];
                }
                return null;
            },
            set(obj, prop, val) {
                if (protocolList.Write.includes(prop)) {
                    obj[prop] = val;
                }
            }
        });
    }
}

module.exports = tmLoader
