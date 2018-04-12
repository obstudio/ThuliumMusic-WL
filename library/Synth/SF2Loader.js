const fs = require("fs");

class SoundFont {
	constructor(file) {
		const size = fs.statSync(file).size;
		const buffer = Buffer.alloc(size);
		const fd = fs.openSync(file, 'rs');
		fs.readSync(fd, buffer, 0, size, 0);
		const data = buffer.slice(0, size);
		this.head = data.toString('utf8', 0, 4);
		this.size = data.readUIntLE(4, 4);
		this.type = data.toString('utf8', 8, 12);
		this.data = this.getChunk(data.slice(12, 8 + this.size));
	}

	getChunk(buffer) {
		const result = [];
		let head, size, type, data;
		while (buffer.length > 0) {
			let head = buffer.toString('utf8', 0, 4);
			let size = buffer.readUIntLE(4, 4);
			if (size + 4 > buffer.length) {
				console.log('Overflow!');
				break;
			} else if (head == 'LIST') {
				type = buffer.toString('utf8', 8, 12);
				data = this.getChunk(buffer.slice(12, 8 + size));
			} else {
				type = undefined;
				data = buffer.slice(8, 8 + size);
			}
			buffer = buffer.slice(8 + size);
			result.push({head, size, type, data});
		}
		return result;
	}

	information() {
		const data = this.data.find(chunk => chunk.type == 'INFO').data;
		const info = [];
		data.forEach((chunk) => {
			switch (chunk.head) {
				case 'ifil': case 'iver':
					info[chunk.head] = chunk.data.readUIntLE(0, 2).toString() + '.' + chunk.data.readUIntLE(2, 2).toString()
					break;
				case 'isng': case 'irom': case 'INAM':
				case 'ICRD': case 'IENG': case 'IPRD':
				case 'ICOP': case 'ICMT': case 'ISFT':
					info[chunk.head] = chunk.data.toString().replace(/\u0000+/, '');
					break;
			}
		});
		return info;
	}

	Preset() {
		const data = this.data.find(chunk => chunk.type == 'pdta').data;
		const pdta = {};
		data.forEach((chunk) => {
			switch (chunk.head) {
				case 'phdr':
					pdta.PresetHeader = {
						// 疑似有问题（38-76）
						PresetName: chunk.data.toString('utf8', 0, 20).replace(/\u0000+/, ''),
						Preset: chunk.data.readUIntLE(20, 2),
						Bank: chunk.data.readUIntLE(22, 2),
						PresetBagNdx: chunk.data.readUIntLE(24, 2),
						Library: chunk.data.readUIntLE(26, 4),
						Genre: chunk.data.readUIntLE(30, 4),
						Morphology: chunk.data.readUIntLE(34, 4)
					}; break;
				case 'pbag':
					pdta.PresetBag = {
						GenNdx: chunk.data.readUIntLE(0, 2),
						ModNdx: chunk.data.readUIntLE(2, 2)
					}; break;
				case 'pmod':
					pdta.ModList = {
						ModSrcOper: chunk.data.readUIntLE(0, 2),
						ModDestOper: chunk.data.readUIntLE(2, 2),
						ModAmount: chunk.data.readUIntLE(4, 2),
						ModAmtSrcOper: chunk.data.readUIntLE(6, 2),
						ModTransOper: chunk.data.readUIntLE(8, 2)
					}; break;
				case 'pgen':
					pdta.GenList = {
						// 疑似有问题（6-124）
						GenOper: chunk.data.readUIntLE(0, 2),
						GenAmount: this.getGenAmount(chunk.data.slice(2))
					}; break;
				case 'inst':
					pdta.Inst = {
						// 疑似有问题（24-44）
						InstName: chunk.data.toString('utf8', 0, 20).replace(/\u0000+/, ''),
						InstBagNdx: chunk.data.readUIntLE(20, 4)
					}; break;
				case 'ibag':
					pdta.InstBag = {
						// 疑似有问题（8-168）
						InstGenNdx: chunk.data.readUIntLE(0, 4),
						InstModNdx: chunk.data.readUIntLE(4, 4)
					}; break;
				case 'imod':
					pdta.InstModList = {
						ModSrcOper: chunk.data.readUIntLE(0, 2),
						ModDestOper: chunk.data.readUIntLE(2, 2),
						ModAmount: chunk.data.readUIntLE(4, 2),
						ModAmtSrcOper: chunk.data.readUIntLE(6, 2),
						ModTransOper: chunk.data.readUIntLE(8, 2)
					}; break;
				case 'igen':
					pdta.InstGenList = {
						// 疑似有问题（6-1256）
						GenOper: chunk.data.readUIntLE(0, 2),
						GenAmount: this.getGenAmount(chunk.data.slice(2))
					}; break;
				case 'shdr':
					pdta.Sample = {
						// 疑似有问题（46-1886）
						SampleName: chunk.data.toString('utf8', 0, 20).replace(/\u0000+/, ''),
						Start: chunk.data.readUIntLE(20, 4),
						End: chunk.data.readUIntLE(24, 4),
						StartLoop: chunk.data.readUIntLE(28, 4),
						EndLoop: chunk.data.readUIntLE(32, 4),
						SampleRate: chunk.data.readUIntLE(36, 4),
						OriginalKey: chunk.data.readUIntLE(40, 1),
						Correction: chunk.data.toString('utf8', 41, 42).replace(/\u0000+/, ''),
						SampleLink: chunk.data.readUIntLE(42, 2),
						SampleType: this.getSampleLink(chunk.data.slice(44))
					}; break;
			}
		});
		return pdta;
	}

	getGenAmount(buffer) {
		return {
			Ranges: {
				Low: buffer.readUIntLE(0, 1),
				High: buffer.readUIntLE(1, 1)
			},
			Amount2: buffer.readUIntLE(0, 2),
			Amount4: buffer.readUIntLE(0, 4)
		};
	}

	getSampleLink(buffer) {
		return {
			0x0001: 'MonoSample',
			0x0002: 'RightSample',
			0x0004: 'LeftSample',
			0x0008: 'LinkedSample',
			0x8001: 'RomMonoSample',
			0x8002: 'RomRightSample',
			0x8004: 'RomLeftSample',
			0x8008: 'RomLinkedSample'
		}[buffer.readUIntLE(0, 2)];
	}
}

module.exports = {SoundFont};

let data = new SoundFont('BrightPiano.sf2')
// console.log(data.Preset().ModList);
console.log(data.Preset());


