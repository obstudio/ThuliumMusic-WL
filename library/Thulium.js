const fs = require('fs');
const Parser = require('./Parser/Parser');
const Tokenizer = require('./Token/Tokenizer');

class Thulium {
	constructor(input, spec = 'URL') {
		this.Tokenizer = new Tokenizer(input, spec);
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
const test = new Thulium('E:/#Obstudio#/QingyunMusicPlayer/Songs/Touhou/test.tm');
// console.log(test.Tokenizer.toParser().Sections[5].Tracks[1].Content[73]);
// console.log(test.parse()[5].Tracks[2].Content.slice(-20).map(t=>t.Pitch).join(', '))

