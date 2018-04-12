const fs = require('fs');
const Parser = require('./Parser/Parser');
const Tokenizer = require('./Token/Tokenizer');

class Thulium {
	constructor(input, {spec = 'URL', buffer = true} = {}) {
		this.Tokenizer = new Tokenizer(input, {spec: spec, buffer: buffer});
		this.$parse = false;
	}

	parse(forced = false) {
		if (this.$parse && !forced) return;
		this.Parser = new Parser(this.Tokenizer.toParser());
		return this.Parser.parse();
		this.$parse = true;
	}
}

module.exports = Thulium;
// const test = new Thulium('E:/#Obstudio#/QingyunMusicPlayer/Songs/Touhou/test.tm');
// console.log(test.parse())


