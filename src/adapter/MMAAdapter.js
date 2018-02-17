/**
 * @class
 * @implements {SMML.Adapter}
 */
class MMAAdapter {
    /**
     * 
     */
    constructor() {
    }

    adapt(parsedSection) {
        let prevTime = 0
        const result = []
        const noFadeTrack = {
            Meta: {
                FadeIn: 0,
                FadeOut: 0,
                Duration: 0
            },
            Content: []
        }
        for (const section of parsedSection) {
            const durs = []
            for (const track of section.Tracks) {
                durs.push(track.Meta.Duration)
                if (track.Meta.FadeIn === 0 && track.Meta.FadeOut === 0) {
                    noFadeTrack.Content.push(...track.Content)
                    noFadeTrack.Meta.Duration += track.Meta.Duration
                } else {
                    result.push(track)
                }
                for (const note of track.Content) {
                    note.Instrument = track.Instrument
                    note.StartTime += prevTime
                }
            }
            prevTime += Math.max(...durs)
        }
        if (noFadeTrack.Meta.Duration > 0) {
            result.push(noFadeTrack)
        }
        return result
    }
}

module.exports = MMAAdapter
