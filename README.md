# Thulium Music Player (Wolfram Language Version)

<div width="50%" style="overflow-x: auto;">
  <img src="https://github.com/obstudio/ThuliumMusicPlayer-WL/blob/master/changes/Logo.svg"/>
<div>

Thulium Music Player is a simple program aimed to play textualized music on any platforms or devices.    
铥铥播放器是一个旨在在任何平台和设备上播放文本化音乐的程序。

This repository is a Wolfram Language Version for Mathematica users.    
这个仓库是该播放器的 Wolfram 语言版本，为 Mathematica 用户使用。

Watch our [propaganda film](https://www.bilibili.com/video/av22536013) on Bilibili.    
在 Bilibili 上观看我们的[宣传片](https://www.bilibili.com/video/av22536013)。

## Installation / 安装

> The following steps require Internet connection. 
> 下面的步骤将需要网络链接。

To run Thulium Music Player, you need to install **[Mathematica](http://www.wolfram.com/mathematica/)** and **[Node.js](https://nodejs.org/)** first.    
为了运行铥铥播放器，你需要首先安装 **[Mathematica](http://www.wolfram.com/mathematica/)** 和 **[Node.js](https://nodejs.org/)**。

After installation, you need to switch to the `library` directory, open the terminal and run the following commands:    
安装完成后，你需要切到`library`目录下，打开终端并输入下列指令：

+ For Windows:

```Command
npm init
cd %USERPROFILE%
npm i zeromq -g
ren node_modules .node_modules
```

+ For MacOS and Linux:

```Command
npm init
cd ~
npm i zeromq -g
mv node_modules .node_modules
```

Then you are ready to run Thulium Music Player. Open `Thulium.nb` with Mathematica, and follow the hint to enjoy the music.    
这样就完成了所有的安装步骤。现在你可以点开`Thulium.nb`文件，按照软件中的提示来欣赏音乐了。

> When the program is first run, it may takes some time to initialize. 
> 当程序首次运行时，可能要花费一定的初始化时间。

## Exception handling / 异常处理

If the program reports an error and does not run properly, you may try the following steps.    
当程序在运行时报告了错误信息并无法运行，你可以尝试采取下面的步骤。

1. Check your Mathematica version. Make sure it is updated.    
请检查你当前的 Mathematica 版本，确保它足够新以便能够执行程序中的所有命令。

> Thulium Music Player uses some new functions in Mathematica 11.2, such as [ExternalEvaluate](http://reference.wolfram.com/language/ref/ExternalEvaluate.html) and [AudioStream](http://reference.wolfram.com/language/ref/AudioStream.html).
> 铥铥播放器中使用了一些 Mathematica 11.2 中引进的新函数，包括 [ExternalEvaluate](http://reference.wolfram.com/language/ref/ExternalEvaluate.html) 和 [AudioStream](http://reference.wolfram.com/language/ref/AudioStream.html)。

2. Make sure **Node.js** and related packages has been installed. You can run a simple Mathematica program to check:    
请确保 **Node.js** 以及相关的库已经被妥善地安装好了。你可以运行下面的 Mathematica 程序来测试是否已经全部安装完成。

```Mathemetica
session = StartExternalSession["NodeJS"]
ExternalEvaluate[session, "require('acorn')"]
```

3. Submit an [issue](https://github.com/obstudio/ThuliumMusic-WL/issues) with the question you have.    
你也可以给我们提交一个 [issue](https://github.com/obstudio/ThuliumMusic-WL/issues)，在其中说明你遇到的问题。

## Change log / 更新日志

- [2.4](https://github.com/obstudio/ThuliumMusicPlayer-WL/blob/master/changes/2.4.md) (2018.5.17)
- [2.3](https://github.com/obstudio/ThuliumMusicPlayer-WL/blob/master/changes/2.3.md) (2018.5.5)
- [2.2](https://github.com/obstudio/ThuliumMusicPlayer-WL/blob/master/changes/2.2.md) (2018.4.21)
- [2.1](https://github.com/obstudio/ThuliumMusicPlayer-WL/blob/master/changes/2.1.md) (2018.4.1)

