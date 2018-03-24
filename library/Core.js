// Core Functions

String.prototype.iCase = function(regexp) {
	const regex = new RegExp(regexp.source, 'g');
	let result = regex.exec(this);
	const output = [];
	while(result != null) {
		output.push(result);
		result = regex.exec(this);
	}
	return output;
};

String.prototype.iPosition = function(test, start = 0) {
	let regexp;
	if (test instanceof RegExp) {
		regexp = new RegExp(test.source, 'g');
	} else if (typeof test == "string") {
		regexp = new RegExp(test, 'g');
	}
	const output = [];
	let string = this.slice(start);
	let pointer = start - 1;
	let result = string.search(regexp);
	while (result != -1) {
		pointer += result + 1;
		output.push(pointer);
		string = string.slice(result + 1);
		result = string.search(regexp);
	}
	return output;
};

String.prototype.iBalance = function(spec, start = 0) {
	const leftCount = this.iPosition('\\' + spec.charAt(0), start).length;
	const rightCount = this.iPosition('\\' + spec.charAt(1), start).length;
	return leftCount - rightCount;
}

String.prototype.iBalanceQ = function() {
	return this.iBalance('()') && this.iBalance('[]') && this.iBalance('{}');
}

Array.prototype.iRiffle = function(sep) {
	if (this.length == 0) return [];
	const output = [this[0]];
	this.forEach((item) => output.push(sep, item));
	return output;
}

module.exports = {};

