class TmError {
    constructor(err, pos, arg) {
        this.name = err
        this.arg = arg
        this.pos = pos === null ? [] : [pos]
    }

    toJSON() {
        return {
            Err: this.name,
            Arg: this.arg,
            Pos: this.pos
        }
    }
}

TmError.Types = {
    Section: {
        DiffDuration: 'Section::DiffDuration',
        InitiativeBar: 'Section::InitiativeBar',
        FinalBar: 'Section::FinalBar',
        Mismatch: 'Section::Mismatch'
    },
    Track: {
        BarLength: 'Track::BarLength',
        UnexpVolta: 'Track::UnexpVolta',
        UnexpCoda: 'Track::UnexpCoda',
        MultiCoda: 'Track::MultiCoda',
        MultiVolta: 'Track::MultiVolta',
        Instrument: 'Track::Instrument',
        Undefined: 'Track::Undefined',
        FadeOverLong: 'Track::FadeOverLong'
    },
    Note: {
        ChordRange: 'Note::ChordRange',
        ChordOverride: 'Note::ChordOverride',
        Reduplicate: 'Note::Reduplicate',
        OutOfRegister: 'Note::OutOfRegister',
        ScaleDegree: 'Note::ScaleDegree',
        NoPrevious: 'Note::NoPrevious',
        VolumeLimit: 'Note::VolumeLimit'
    },
    Arg: {
        OutOfRange: 'Arg::OutOfRange',
        UniNote: 'Arg::UniNote',
        NotLog2: 'Arg::NotLog2',
        WrongType: 'Arg::WrongType'
    }
}

module.exports = TmError
