// Prelude No.1 in C Major
// BMV 846
// J.S.Bach

# Include Benzene

# Chord

B	[], 1
w	0, 3, 5
e	0, 2, 5
E	0, 2, 6

# Function

function Distribute(template, subtrack) {
	// Alias: ${0:sub}->${1:sub}
	const temp = this.ParseTrack(template);
	const src = this.ParseTrack(subtrack, { Settings: this.newSettings() });
	const content = [];
	temp.Content.forEach(tNote => {
		const scale = tNote.Duration / src.Meta.Duration;
		content.push(...src.Content.map(sNote => Object.assign({}, sNote, {
			Pitch: tNote.Pitch + sNote.Pitch,
			Duration: sNote.Duration * scale,
			StartTime: sNote.StartTime * scale + tNote.StartTime,
			Volume: sNote.Volume * tNote.Volume
		})));
	});
	return Object.assign(temp, {Content: content});
}

# End

(1=C) (65) (4/4)

<:s:> { 1_0.1_0. } -> $~ @this

<:t:> { Dur(1) 01.1.01.1. } -> $~ @this

<Piano> Extend({
	@t?=1Mj | @t?=2mj | @t?=2wj | @t?=1Mj |
	@t?=6po | @t?=2Mi | @t?=5po | @t?=1Mi |
	@t?=1Mi | @t?=1Ei | @t?=5,Mj| @t?=1#di|
	@t?=2po | @t?=1ei | @t?=1po | @t?=4,Mi|
	Oct(-1)
	@t?=4Mi | @t?=4Ei | @t?=1Mj | @t?=7bE |
	@t?=6m  | @t?=6d  | @t?=7Bt | @t?=5M  |
	@t?=1Mj | @t?=5ps | @t?=4Ei | @t?=4#di|
	@t?=5qo | @t?=5ps | @t?=4Ei | @t?=3di |
}, 0.5) Oct(-1)
Dur(2) 0-461'4'1'61'6464242|
Oct(0) 0-572'4'2'72'7572432|
Dur(0) 1Mi---|

<Piano> Extend({ Oct(-1)
	@s?=1'T | @s?=1'D | @s?=7t  | @s?=1'T |
	@s?=1'T | @s?=1'D | @s?=7t  | @s?=7B  |
	@s?=6t  | @s?=2p  | @s?=5T  | @s?=5t  |
	@s?=4T  | @s?=4t  | @s?=3t  | @s?=3B  |
	@s?=2t  | @s?=5,p | @s?=1T  | @s?=1p  |
	Oct(-2)
	@s?=4o  | @s?=4#Q | @s?=5#H | @s?=5s  |
	@s?=5H  | @s?=5p  | @s?=5p  | @s?=5h  |
	@s?=5H  | @s?=5p  | @s?=5p  | @s?=1o  |
}, 1.5) Oct(-2) Seg(1) (80%)
$1o---|$1o---|1o---|


