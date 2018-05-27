const fs = require('fs')
const Thulium = require('./Thulium')
const Adapter = require('./Adapter/Adapter')
const instrDict = require('./Config/Instrument.json')

class TmRedstone {
  constructor({source, dest, spec = []}) {
    this.name = source.match(/\w+$/)[0]
    this.src = new Thulium(__dirname + '/../Songs/' + source + '.tm');
    this.data = new Adapter(this.src.parse(), ...spec).adapt()
    this.dir = dest + '/data/functions/tm/' + this.name
  }

  generate() {
    let output = `\
      # Thulium Music for Minecraft 1.0
      # ${this.src.Comment.join('\n # ')}\n`
    output += this.data.map((data, index) => {
      const y0 = 4 * Math.floor(index / 2) + 4
      const result = index % 2 === 0
        ? this.generateTrack(data, index, 1, 2, y0, 1)
        : this.generateTrack(data, index, -1, 2, y0, 1)
      return `
        function tm:${this.name}_${index}
        setblock ~-2 ${y0 - 1} ~-2 stone_slab 8
        setblock ~-2 ${y0} ~-2 redstone_wire
        setblock ~2 ${y0 - 1} ~-2 stone_slab 8
        setblock ~2 ${y0} ~-2 redstone_wire 
        ${y0 === 4 ? '' : `
          setblock ~-1 ${y0 - 2} ~-2 stone_slab 8
          setblock ~-1 ${y0 - 1} ~-2 redstone_wire
          setblock ~1 ${y0 - 2} ~-2 stone_slab 8
          setblock ~1 ${y0 - 1} ~-2 redstone_wire
          setblock ~ ${y0 - 3} ~-2 stone_slab 8
          setblock ~ ${y0 - 2} ~-2 redstone_wire
          setblock ~-1 ${y0 - 4} ~-2 stone_slab 8
          setblock ~-1 ${y0 - 3} ~-2 redstone_wire
          setblock ~1 ${y0 - 4} ~-2 stone_slab 8
          setblock ~1 ${y0 - 3} ~-2 redstone_wire
        `}`
    }).join('')
    output += `
      setblock ~-2 4 ~-3 redstone_wire
      setblock ~-1 4 ~-3 redstone_wire
      setblock ~ 4 ~-3 redstone_wire
      setblock ~1 4 ~-3 redstone_wire
      setblock ~2 4 ~-3 redstone_wire
      setblock ~ 4 ~-4 unpowered_repeater 14
      setblock ~ 4 ~-5 unpowered_repeater 14
      setblock ~ 4 ~-6 unpowered_repeater 14
      setblock ~ 4 ~-7 stone_button 5`
    fs.writeFileSync(this.dir + '.mcfunction', output.replace(/^ +/gm, ''), {encoding: 'utf8'});
  }

  generateTrack(track, id, d, x0, y0, dv) {
    const inst = track.Name.match(/\.(\w*)\./)[1]
    const instID = instrDict[inst] + 1
    const scale = track.Settings.Speed / 15
    const result = []
    for (let i = 0; i < Math.round(track.Meta.Duration * scale); i++) {
      result.push([])
    }
    track.Content.forEach(item => {
      const index = Math.round(item.StartTime * scale)
      result[index].push({
        Pitch: item.Pitch,
        Volume: dv * item.Volume
      })
    })
    let output = `\
      # Track: ${id}
      # Instrument: ${inst}\n`
    for (let i = 0; i < result.length; i++) {
      const z = 2 * Math.floor(i / 4)
      const x = d * (x0 + 2 * (i % 4))
      output += i % 4 === 0
        ? `
          setblock ~${x} ${y0 - 1} ~${z - 1} stone_slab 8
          setblock ~${x} ${y0} ~${z - 1} unpowered_repeater 14`
        : `
          setblock ~${x - d} ${y0 - 1} ~${z} stone_slab 8
          setblock ~${x - d} ${y0} ~${z} unpowered_repeater ${2 - d}`
      output += `\nsetblock ~${x} ${y0 - 1} ~${z} stone_slab 8`
      output += result[i].length === 0
        ? `\nsetblock ~${x} ${y0} ~${z} redstone_wire`
        : result[i].map((item, j) => `
          setblock ~${x} ${y0 + j} ~${z} ${j === 0 ? '' : 'chain_'}command_block 1
          blockdata ~${x} ${y0 + j} ~${z} {Command: "playsound ${instID}.${60 + item.Pitch} master @p ~ ~ ~ ${item.Volume}"}`
        ).join('')
      output += '\n'
    }
    fs.writeFileSync(this.dir + `_${id}.mcfunction`, output.replace(/^ +/gm, ''), {encoding: 'utf8'})
    return true
  }
}

new TmRedstone({
  source: 'Touhou/test4',
  dest: 'D:/Minecraft/.minecraft/saves/test'
}).generate()
