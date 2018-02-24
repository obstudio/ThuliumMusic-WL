class SMMLError extends Error {
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

class BarLengthError extends SMMLError{
    constructor(id, index, length) {
        super('BarLength', '', id, index, length)
    }
}

class DupChordError extends SMMLError{
    constructor(id, index, pitches) {
        super('DupChord', '', id, index, pitches)
    }
}

class TraceError extends SMMLError{
    constructor(id, index, trace) {
        super('Trace', '', id, index, trace)
    }
}

class VolumeError extends SMMLError{
    constructor(id, index, volume) {
        super('Trace', '', id, index, volume)
    }
}

module.exports = {
    BarLengthError,
    DupChordError,
    TraceError,
    VolumeError
}
