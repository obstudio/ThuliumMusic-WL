const mask = 0x7F

class MIDIEvent {
    /**
     * Deal with Variable-Length number
     * @param {number} number 
     */
    static toVariableLengthFormat (number) {
        const length = Math.ceil(number.toString(2).length / 7)
        const uArray = new Uint8Array(length)
        for (let i = length - 1; i >= 0; i--) {
            const offset = 7 * (length - i - 1)
            uArray[i] = ((number >> offset) & mask) + ((i === length - 1) ? 0 : 0x80)
        }
        return {
            origin: number,
            length,
            uArray
        }
    }

    constructor (deltaTime) {
        this.deltaTime = MIDIEvent.toVariableLengthFormat(deltaTime)
    }

    /**
     * @abstract
     */
    toUint8Array() {
        return this.uArray
    }
}

class MIDIChannelEvent extends MIDIEvent {
    /**
     * Construct a MIDI event
     * @param {number} deltaTime 
     * @param {number} type 
     * @param {number} channel 
     * @param {number} param1 
     * @param {number} param2 
     */
    constructor (deltaTime, type, channel, param1, param2) {
        super(deltaTime)
        this.type = type
        this.channel = channel
        this.param1 = param1
        this.param2 = param2
        this.totalLength = this.deltaTime.length + 3
        this.uArray = new Uint8Array(this.totalLength)
        this.uArray.set(this.deltaTime.uArray)
        this.uArray.set([type << 4 + channel, param1, param2], this.deltaTime.length)
    }
}

class MIDIMetaEvent extends MIDIEvent {
    /**
     * Construct a MIDI event
     * @param {number} deltaTime 
     * @param {number} type 
     * @param {Uint8Array} uArray 
     */
    constructor (deltaTime, type, uArray) {
        super(deltaTime)
        this.type = (0xFF << 2) + type
        this.uArrayParam = uArray
        this.length = MIDIEvent.toVariableLengthFormat(this.uArrayParam.length)

        this.totalLength = this.deltaTime.length + 2 + this.length.length + this.uArrayParam.length
        this.uArray = new Uint8Array(this.totalLength)
        this.uArray.set(this.deltaTime.uArray)
        this.uArray.set([0xFF, type, ...this.length.uArray, ...this.uArrayParam], this.deltaTime.length)
    }
}

class MIDISysExEvent extends MIDIEvent {
    /**
     * Construct a MIDI event
     * @param {number} deltaTime 
     * @param {number[]} type 
     * @param {Uint8Array} uArray 
     */
    constructor (deltaTime, type, uArray) {
        super(deltaTime)
        this.head = type[0]
        this.tail = type[1]
        this.uArrayParam = uArray
        this.length = MIDIEvent.toVariableLengthFormat(this.uArrayParam.length)
        if (this.tail === null) {
            this.totalLength = this.deltaTime.length + 1 + this.length.length + this.uArrayParam.length
            this.uArray = new Uint8Array(this.totalLength)
            this.uArray.set(this.deltaTime.uArray)
            this.uArray.set([this.head, ...this.length.uArray, ...this.uArrayParam], this.deltaTime.length)
        } else {
            this.totalLength = this.deltaTime.length + 2 + this.length.length + this.uArrayParam.length
            this.uArray = new Uint8Array(this.totalLength)
            this.uArray.set(this.deltaTime.uArray)
            this.uArray.set([this.head, ...this.length.uArray, ...this.uArrayParam, this.tail], this.deltaTime.length)
        }
    }
}

class MIDIEventFactory {
    static createMIDIChannelEvent(deltaTime, subType, channel, param1, param2) {
        return new MIDIChannelEvent(deltaTime, subType, channel, param1, param2)
    }

    static createMetaEvent(deltaTime, subType, uArray) {
        return new MIDIMetaEvent(deltaTime, subType, uArray)
    }

    static createSysExEvent(deltaTime, subType, uArray) {
        return new MIDISysExEvent(deltaTime, subType, uArray)
    }
}

MIDIEventFactory.midiSubType = {
    noteOff: 0x8,
    noteOn: 0x9,
    noteAftertouch: 0xA,
    controlChange: 0xB,
    programChange: 0xC,
    channelAftertouch: 0xD,
    pitchBend: 0xE
}

MIDIEventFactory.metaSubType = {
    sequenceNumber: 0x00,
    text: 0x01,
    copyrightNotice: 0x02,
    trackName: 0x03,
    instrumentName: 0x04,
    lyrics: 0x05,
    marker: 0x06,
    cuePoint: 0x07,
    midiChannelPrefix: 0x20,
    endOfTrack: 0x2F,
    setTempo: 0x51,
    smpteOffset: 0x54,
    timeSignature: 0x58,
    keySignature: 0x59,
    sequencerSpecific: 0x7F
}

MIDIEventFactory.sysExSubType = {
    normal: [0xF0, 0xF7],
    initial: [0xF0, null],
    upcoming: [0xF7, null],
    ending: [0xF7, 0xF7]
}

module.exports = MIDIEventFactory