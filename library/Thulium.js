const fs = require('fs');
const Parser = require('./Parser/Parser');
const Tokenizer = require('./Token/Tokenizer');

const packagePath = __dirname + '/../package/';
const packageInfo = require(packagePath + 'index.json');
const library = { Path: packagePath, ...packageInfo };

function loader(path, buffer = false) {
	if (buffer && fs.existsSync(path + '/buffer.json')) {
		packageData = require(path + '/buffer.json');
	} else {
		const content = fs.readFileSync(path + '/main.tml', 'utf8');
		packageData = new Tokenizer(content, loader, library).getLibrary();
		fs.writeFileSync(path + '/buffer.json', JSON.stringify(packageData), 'utf8');
	}
	return packageData;
};

class Thulium {
	constructor(input, { spec = 'URL', buffer = false } = {}) {
		if (spec = 'URL') {
			input = fs.readFileSync(input, 'utf8');
		}
		const tokenizer = new Tokenizer(input, loader, library);
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
