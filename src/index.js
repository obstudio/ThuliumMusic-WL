const Parser = require('./Parser')
const MMAAdapter = require('./adapter/MMAAdapter')
const MIDIAdapter = require('./adapter/MIDIAdapter')

Error.prototype.toJSON = function () {
    return {
        message: this.message
    }
}

module.exports = {
    Parser,
    MMAAdapter,
    MIDIAdapter
}
