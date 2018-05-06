const fs = require('fs');
const path = require('path');
const Parser = require('./Parser/Parser');
const Tokenizer = require('./Token/Tokenizer');
const Linter = require('./Linter/Linter');

const packagePath = __dirname + '/../package';
const packageInfo = require(packagePath + '/index.json');
const library = { Path: packagePath, ...packageInfo };

class Thulium {
	constructor(input, { useFile = true, buffer = false } = {}) {
		if (useFile) {
			var directory = path.dirname(input);
			input = fs.readFileSync(input, 'utf8');
		}
		function loadFile(filename) {
			if (fs.existsSync(filename + '.tml')) {
				const content = fs.readFileSync(filename + '.tml', {encoding: 'utf8'});
				return new Tokenizer(content, {
					loadFile: loadFile,
					$library: library,
					$directory: path.dirname(filename)
				}).initialize();
			} else if (fs.existsSync(filename)) {
				if (buffer && fs.existsSync(filename + '/buffer.json')) {
					return require(filename + '/buffer.json');
				} else if (fs.existsSync(filename + '/main.tml')) {
					const content = fs.readFileSync(filename + '/main.tml', {encoding: 'utf8'});
					const data = new Tokenizer(content, {
						loadFile: loadFile,
						$library: library,
						$directory: filename
					}).initialize();
					fs.writeFileSync(filename + '/buffer.json', JSON.stringify(data), {encoding: 'utf8'});
					return data;
				} else {
					throw new Error(`File "${filename}/main.tml" was not found!`);
				}
			} else {
				throw new Error(`File "${filename}.tml" was not found!`);
			}
		}
		const tokenizer = new Tokenizer(input, {
			loadFile: loadFile,
			$library: library,
			$directory: directory
		});
		tokenizer.tokenize();
		Object.assign(this, tokenizer);
		this.$parse = false;
	}

	parse(forced = false) {
		if (this.$parse && !forced) return this.MusicClips; 
		this.MusicClips = new Parser(this).parse();
		this.$parse = true;
		return this.MusicClips;
	}

	detokenize() {
		return new Linter(this.Tokenizer, this.Syntax).detokenize();
	}

	attributes(attrs) {
		const result = {};
		attrs.forEach(attr => result[attr] = this[attr]);
		return result;
	}

	get Tokenizer() {
		return this.attributes(['Comment', 'Library', 'Sections', 'Warnings']);
	}

	get information() {
		const clips = this.parse(true);
		const comment = this.Comment.map(line => {
			const result = [];
			line = line.trim();
			for (let i = 0; i < line.length; i++) {
				result.push(line.charCodeAt(i));
			}
			return result;
		});
		return {
			Status: 'Succeed',
			Comment: comment,
			Sections: this.Sections,
			Warnings: this.Warnings,
			MusicClips: clips
		}
	}
}

module.exports = Thulium;

