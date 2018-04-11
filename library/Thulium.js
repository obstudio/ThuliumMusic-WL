const fs = require('fs');
const Parser = require('./Parser/Parser');
const Tokenizer = require('./Token/Tokenizer');

class Thulium {
	constructor(input, spec = 'URL') {
		this.Tokenizer = new Tokenizer(input, spec);
		this.Parser = new Parser(this.Tokenizer.toParser());
	} 
	parse() {
		return this.Parser.parse();
	}
}

// const test = new Thulium('../Songs/Touhou/test.tm');
// console.log(test.Tokenizer.Syntax.Chord[17].Pitches);

