const fs = require('fs')
const process = require('process')
const path = require('path')
const { Parser, MIDIAdapter, MMAAdapter } = require('./index')
const typeDict = {
    Normal: undefined,
    MIDI: MIDIAdapter, 
    MMA: MMAAdapter
}

const input = process.argv[2]
if (!input || (process.argv[3] && !(process.argv[3] in typeDict))) {
    // eslint-disable-next-line no-console
    console.log('Usage: node index.js <input_path> [adapter_type]')    
    process.exit(0)
}

const typeStr = process.argv[3] || 'Normal'
const type = typeDict[typeStr]

const output = input.split('.').slice(0, -1).concat([typeStr, 'json']).join('.')

fs.readFile(path.join(path.resolve('./'), input), 'utf8', (err, data) => {
    const jsonData = JSON.parse(data)
    let parser
    if (type) {
        parser = new Parser(jsonData, new type())
    } else {
        parser = new Parser(jsonData)
    }
    const json = JSON.stringify(parser.parse())
    fs.writeFile(path.join(path.resolve('./'), output), json, 'utf8', () => {})
})