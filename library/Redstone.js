const fs = require('fs')
const Thulium = require('./Thulium')
const Adapter = require('./Adapter/Adapter')
const instrDict = require('./Config/Instrument.json')

function setblock(x, y, z, block = 'redstone_wire', info = '') {
  return `
    setblock ~${x} ${y - 1} ~${z} stone_slab 8
    setblock ~${x} ${y} ~${z} ${block} ${info}`
}

function setcommand(x, y, z, data, inst) {
  return data.map((note, index) => `
    setblock ~${x} ${y + index} ~${z} ${index === 0 ? '' : 'chain_'}command_block 1
    blockdata ~${x} ${y + index} ~${z} {Command: "execute @p ~ ~ ~ playsound ${inst}.${60 + note.Pitch} master @p ~ ~ ~ ${note.Volume}"}`
  ).join('')
}

class TmRedstone {
  constructor({source, dest, spec = []}) {
    this.name = source.match(/\w+$/)[0]
    this.src = new Thulium(__dirname + '/../Songs/' + source + '.tm');
    this.data = new Adapter(this.src.parse(), ...spec).adapt()
    this.dir = dest + '/data/functions/' + this.name + '/'
    if (!fs.existsSync(this.dir)) fs.mkdirSync(this.dir)
  }

  generate() {
    let output = `\
      # Thulium Music for Minecraft 1.0
      # ${this.src.Comment.join('\n # ')}\n`
    output += this.data.map((data, index) => {
      const z0 = Math.floor(index % 4 / 2)
      const y0 = 4 * Math.floor(index / 4) + 4 + z0 * 2
      this.generateTrack(data, index, {
        d: 1 - 2 * (index % 2), 
        x0: 2 + z0,
        y0: y0,
        z0: z0,
        dv: 1
      })
      let result = `\nfunction ${this.name}:track_${index}`
      if (y0 !== 4 && z0 === 0) {
        result += setblock(-3, y0 - 1, -2)
          + setblock(3, y0 - 1, -2)
          + setblock(-2, y0 - 2, -2)
          + setblock(2, y0 - 2, -2)
          + setblock(-3, y0 - 3, -2)
          + setblock(3, y0 - 3, -2)
      }
      return result
    }).join('\n')
    output += `\n
      setblock ~-2 4 ~-3 redstone_wire
      setblock ~-1 4 ~-3 redstone_wire
      setblock ~ 4 ~-3 redstone_wire
      setblock ~1 4 ~-3 redstone_wire
      setblock ~2 4 ~-3 redstone_wire
      setblock ~ 4 ~-4 unpowered_repeater 14
      setblock ~ 4 ~-5 unpowered_repeater 14
      setblock ~ 4 ~-6 unpowered_repeater 14
      setblock ~ 4 ~-7 stone_button 5`
    fs.writeFileSync(this.dir + 'build.mcfunction', output.replace(/^ +/gm, ''), {encoding: 'utf8'});
  }

  generateTrack(track, id, {d, x0, y0, z0, dv}) {
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
      # Instrument: ${inst}
      ${setblock(x0 * d, y0, z0 - 2)}`
    for (let i = 0; i < result.length; i += 4) {
      let x = d * x0, y = y0, z = z0 + i / 2, s = 0
      output += setblock(x, y, z - 1, 'unpowered_repeater', 14)
      output += `\nsetblock ~${x} ${y - 1} ~${z} stone_slab 8`
      output += result[i].length === 0
        ? `\nsetblock ~${x} ${y} ~${z} redstone_wire`
        : setcommand(x, y, z, result[i], instID)

      for (let j = 1; j < 4; j++) {
        s += 1
        if (result[i + j].length !== 0) {
          x += 2 * d
          output += setblock(x - d, y, z, 'unpowered_repeater', 4 * s - 2 - d)
            + `\nsetblock ~${x} ${y - 1} ~${z} stone_slab 8`
            + setcommand(x, y, z, result[i + j], instID)
          s = 0
        }
      }
    }
    fs.writeFileSync(this.dir + `track_${id}.mcfunction`, output.replace(/^ +/gm, ''), {encoding: 'utf8'})
    return true
  }
}

new TmRedstone({
  source: 'Clannad/Nagisa',
  dest: 'D:/Minecraft/.minecraft/saves/test'
}).generate()
