const fs = require('fs');
const Parser = require('./Parser/Parser');
const Tokenizer = require('./Token/Tokenizer');

class Thulium {
	constructor(input, { spec = 'URL', buffer = false } = {}) {
		const tokenizer = new Tokenizer(input, { spec, buffer });
		tokenizer.tokenize();
		Object.assign(this, tokenizer);
		this.$parse = false;
	}

	parse(forced = false) {
		if (this.$parse && !forced) return; 
		this.MusicClips = new Parser(this).parse();
		this.$parse = true;
		return this.MusicClips;
	}

	attributes(...attrs) {
		const result = {};
		attrs.forEach(attr => result[attr] = this[attr]);
		return result;
	}
}

module.exports = Thulium;
// const test = new Thulium('E:/#Obstudio#/QingyunMusicPlayer/Songs/Levan_Polkka.tm');
// const test = new Thulium('E:/#Obstudio#/QingyunMusicPlayer/Songs/Touhou/test.tm');
// console.log(test.parse()[0])

// console.log(test.Tokenizer.toParser().Sections[0].Tracks[5])
// console.log(test.Tokenizer.Syntax.Alias)


