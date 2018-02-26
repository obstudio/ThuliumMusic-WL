## Qingyun Music Player (Wolfram Language Version)

A simple program aimed to play electronic music on any platforms or devices.

## Installation

To run Qingyun Music Player, you need to install **Node.js**.

[Node.js 8.9.4 LTS](https://nodejs.org/dist/v8.9.4/node-v8.9.4-x64.msi)

After installation, run Node.js command prompt:

```Command
npm install zeromq
ren node_modules .node_modules
```

## Why my Mathematica reports an error?

1. Check your Mathematica version. Make sure it is updated.

> Qingyun Music Player uses some new functions in Mathematica 11.2, such as [ExternalEvaluate](http://reference.wolfram.com/language/ref/ExternalEvaluate.html)and [AudioStream](http://reference.wolfram.com/language/ref/AudioStream.html).

2. Make sure **Node.js** and **ZeroMQ** has been installed. You can run a simple Mathematica program to check:

```Mathemetica
FindExternalEvaluators["NodeJS"]
```
