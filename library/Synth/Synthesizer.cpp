#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define Pi 3.14159265358979323846264338327950288
#define LegalChar(chr) (chr > ' ' && chr <= 'z')
#define StrEqual(str1, str2) (strcmp(str1, str2) == 0)
#define PrintStr(str) (printf("%s\n", str))

typedef char c4[4];
typedef char c20[20];
typedef signed __int8 s8;
typedef unsigned __int8 u8;
typedef signed __int16 s16;
typedef unsigned __int16 u16;
typedef signed __int32 s32;
typedef unsigned __int32 u32;

int stdRead(FILE* f, void* ptr, unsigned int size) {
	return (int)fread(ptr, 1, size, f);
}

int stdSkip(FILE* f, unsigned int count) {
	return !fseek(f, count, SEEK_CUR);
}

int memRead(fsmem m, void* ptr, unsigned int size) {
	if (size > m->total - m->pos) size = m->total - m->pos;
	TSF_MEMCPY(ptr, m->buffer+m->pos, size);
	m->pos += size;
	return size;
}
int memSkip(fsmem m, unsigned int count) {
	if (m->pos + count > m->total) return 0;
	m->pos += count;
	return 1;
}
typedef class FSMemory* fsmem;
class FSMemory {
	public:
	const char* buffer;
	unsigned int total, pos;
	SF2 FSMemory(const void* buffer, unsigned int size) {
		fs stream = new FileStream("mem");
		buffer = (const char*)buffer;
		total = size;
		pos = 0;
		stream->data = this;
		return new SoundFont(&stream);
	}
};

typedef class FileStream* fs;
class FileStream {
	private:
	int (*read)(void*, void*, unsigned int);
	int (*skip)(void*, unsigned int);

	public:
	void* data;

	FileStream(const char* type = "std") {
		data = NULL;
		if (StrEqual(type, "std")) {
			read = (int(*)(void*, void*, unsigned int)) &stdRead;
			skip = (int(*)(void*, unsigned int)) &stdSkip;
		} else if (StrEqual(type, "mem")) {
			read = (int(*)(void*, void*, unsigned int)) &memRead;
			skip = (int(*)(void*, unsigned int)) &memSkip;
		}
	}

	ssize_t Read(void* target, unsigned int size) {
		return this->read(data, target, size);
	}
};

typedef class RIFFChunk* chunk;
class RIFFChunk {
	private:
	c4 id;
	u32 size;

	public:
	RIFFChunk(fs stream, chunk parent = NULL) {
		stream->Read(&id, sizeof(c4));
		stream->Read(&size, sizeof(u32));
		if (parent) parent->size -= sizeof(c4) + sizeof(u32) + size;
		stream->Read(&id, sizeof(c4));
		size -= sizeof(c4);
	}
};

typedef class SoundFont* SF2;
class SoundFont {
	private:


	public:
	SoundFont(fs stream) {
		chunk chunkHead = new RIFFChunk(stream, NULL);
	}

};

SF2 SF2Load(const char* filename) {
	fs stream = new FileStream("std");
	FILE* file = fopen(filename, "rb");
	stream->data = file;
	const SF2 resource = new SoundFont(stream);
	fclose(file);
	return resource;
}

int main() {
	SF2 soundfont = SF2Load("SoundFont.sf2");
	return 0;
}
