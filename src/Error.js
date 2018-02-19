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
        super('BarLengthError', '', id, index, length)
    }
}

module.exports = {
    BarLengthError
}