const core = require('../Core.js');

class FuncSyntax {
	constructor(code) {
		let result;
		this.Code = code;
		result = code.iCase(/\/\*{4} (.+) \*{4}\//);
		this.Alias = result.map((item) => item[1].trim());
		result = code.iPosition('return');
		this.Void = result.every((pos) => code.iBalance('{}', pos) != -1);
	}
}

class ChordSyntax {
	constructor(code) {
		const structure = code.split(/\t+/);
		const data = structure[structure.length - 1].split(/, */);
		this.Notation = structure[0];
		if (structure.length == 3) this.Comment = structure[1];
		this.Data = data.map((item) => {
			let match;
			if ((match = /\[([+\-]?\d+)?; *([+\-]?\d+)?\]([+\-]\d+)?/.exec(item)) != null) {
				return [
					parseInt(match[1] || 1),
					parseInt(match[2] || -1),
					parseInt(match[3] || 0)
				];
			} else if ((match = /\[([+\-]?\d+)?\]([+\-]\d+)?/.exec(item)) != null) {
				return [
					parseInt(match[1] || 1),
					parseInt(match[1] || -1),
					parseInt(match[2] || 0)
				];
			} else if ((match = /([+\-]?\d+)/.exec(item)) != null) {
				return [1, 1, parseInt(match[1] || 0)];
			}
		})
	}
}

class MacroSyntax {
	constructor(code) {
		const match = /<\*([a-zA-Z]\w*)\*>([\s\S]*)/.exec(code);
		this.Name = match[1];
		this.Score = match[2];
	}
}

class Syntax {
	constructor() {
		this.Function = [];
		this.Chord = [];
		this.Macro = [];
	}
}

module.exports = {FuncSyntax, ChordSyntax, MacroSyntax}
