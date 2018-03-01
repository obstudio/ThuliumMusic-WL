class TmError extends Error {
    constructor(errID, msg, ...args) {
        super(msg)
        this.name = errID
        this.args = args
    }

    toJSON() {
        return {
            ErrID: this.name,
            Args: this.args,
            // Message: this.message
        }
    }
}

class BarLengthError extends TmError{
    constructor(id, index, length) {
        super('BarLength', '', id, index, length)
    }
}

class DupChordError extends TmError{
    constructor(id, index, pitches) {
        super('DupChord', '', id, index, pitches)
    }
}

class TraceError extends TmError{
    constructor(id, index, trace) {
        super('Trace', '', id, index, trace)
    }
}

class VolumeError extends TmError{
    constructor(id, index, volume) {
        super('Trace', '', id, index, volume)
    }
}

class UndefinedTokenError extends TmError{
    constructor(id, index, token) {
        super('Undef', '', id, index, token)
    }
}

module.exports = {
    BarLengthError,
    DupChordError,
    TraceError,
    VolumeError,
    UndefinedTokenError
}
