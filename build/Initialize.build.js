const fs = require('fs');
const core = require('../Library/Core.js');

class build {
	constructor(name) {
		const filePath = '../package/' + name + '/.init.js';
		const rawData = fs.readFileSync(filePath, {encoding: 'utf8'});
		let code = /module\.exports *= *\{\s([\s\S]*)\s\}/.exec(rawData)[1].split('\r\n');
		const output = [code.shift()];
		while (code.length > 0) {
			if (output[output.length - 1].iBalanceQ()) {
				output.push(code[0]);
			} else {
				output[output.length - 1] += code[0];
			}
			code.shift();
		}
		this.FuncList = output;
	}
}

console.log(new build('Standard').FuncList[0]);

