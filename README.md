# Thulium Music Player (Wolfram Language Version)

A simple program aimed to play electronic music on any platforms or devices.

## Installation

To run Thulium Music Player, you need to install **[Mathematica](http://www.wolfram.com/mathematica/)** and **[Node.js](https://nodejs.org/en/)**.

After installation, run Node.js command prompt:

```Command
npm install zeromq
npm install acorn
ren node_modules .node_modules
```

## Why my Mathematica reports an error?

1. Check your Mathematica version. Make sure it is updated.

> Thulium Music Player uses some new functions in Mathematica 11.2, such as [ExternalEvaluate](http://reference.wolfram.com/language/ref/ExternalEvaluate.html) and [AudioStream](http://reference.wolfram.com/language/ref/AudioStream.html).

2. Make sure **Node.js** and **ZeroMQ** has been installed. You can run a simple Mathematica program to check:

```Mathemetica
FindExternalEvaluators["NodeJS"]
```
