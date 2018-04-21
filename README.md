# Thulium Music Player (Wolfram Language Version)

A simple program aimed to play electronic music on any platforms or devices.

## Installation

> The following steps require Internet connection.

To run Thulium Music Player, you need to install **[Mathematica](http://www.wolfram.com/mathematica/)** and **[Node.js](https://nodejs.org/)** first.

After installation,

+ For Windows, open Node.js command prompt, and run the following commands:

```Command
cd %USERPROFILE%
npm install zeromq
npm install acorn
ren node_modules .node_modules
```

+ For macOS and Linux, open the terminal and run:

```Command
cd ~
npm install zeromq
npm install acorn
mv node_modules .node_modules
```

Then you are ready to run Thulium Music Player. Open Thulium.nb with Mathematica, and follow the hint to enjoy the music.

## Why my Mathematica reports an error?

1. Check your Mathematica version. Make sure it is updated.

> Thulium Music Player uses some new functions in Mathematica 11.2, such as [ExternalEvaluate](http://reference.wolfram.com/language/ref/ExternalEvaluate.html) and [AudioStream](http://reference.wolfram.com/language/ref/AudioStream.html).

2. Make sure **Node.js** and **zeromq** and **acorn** has been installed. You can run a simple Mathematica program to check:

```Mathemetica
FindExternalEvaluators["NodeJS"]
```
