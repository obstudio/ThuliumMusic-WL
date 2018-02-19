const Parser = require('./Parser')

Error.prototype.toJSON = function () {
    return {
        message: this.message
    }
}

module.exports = { Parser }
