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
// const test = new Thulium('E:/#Obstudio#/QingyunMusicPlayer/Songs/Touhou/test.tm');
// console.log(test.Tokenizer.toParser().Sections[1].Tracks[0].Content[5].Content.filter(tok => tok.Type === 'BarLine').map(tok => tok.Order));

