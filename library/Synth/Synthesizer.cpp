#define TSF_IMPLEMENTATION
#include "tsf.h"

int main() {

	tsf* TinySoundFont = tsf_load_filename("BrightPiano.sf2");
	tsf_set_output(TinySoundFont, TSF_MONO, 44100, 0); //sample rate
	tsf_note_on(TinySoundFont, 0, 60, 1.0f); //preset 0, middle C
	short HalfSecond[22050]; //synthesize 0.5 seconds
	tsf_render_short(TinySoundFont, HalfSecond, 22050, 0);

}

