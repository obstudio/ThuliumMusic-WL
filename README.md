# Thulium Music Player (Wolfram Language Version)

<div> 
  <svg>
    <title>Thulium Logo</title>
    <path fill="#00A0E9" d="M836.15,454.53c90.25,18.32,158.21,98.11,158.21,193.791c0,109.22-88.54,197.729-197.75,197.729h-568.5c-122.87,0-222.46-99.6-222.46-222.46c0-96.97,62.07-179.44,148.62-209.9c-0.22-4.15-0.32-8.34-0.32-12.57c0-136.5,110.67-247.17,247.18-247.17c91.59,0,171.54,49.82,214.24,123.84c23.55-15.77,51.88-24.97,82.37-24.97c81.91,0,148.29,66.41,148.29,148.3c0,18.84-3.511,36.84-9.9,53.41H836.15z M569.07,330.54c0,0-3.4-3.95-12.53-10.1c-28.78-19.4-64.45-29.11-98.33-31.92c-10.33-0.86-23.36-1.46-33.73-1.62c-25.17-0.38-53.61,3.05-76.72,14.4L492.81,615.11c-22.67-12.301-52.2-18.33-83.75-15.301c-65.69,6.311-115.29,49.521-110.78,96.53c4.51,47,61.43,79.99,127.13,73.69c65.69-6.311,115.29-49.53,110.78-96.53c-0.53-5.48-1.771-10.77-3.65-15.83h-0.01L424.34,315.88c6.1-2.72,35.51-4.39,52.38-4.1c22.98,0.39,59.32,7.82,69.48,10.78C555.89,325.38,569.07,330.54,569.07,330.54z"/>
  </svg>
</div>

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

- [2.3](https://github.com/obstudio/ThuliumMusicPlayer-WL/blob/master/changes/2.3.md) (2018.5.5)
- [2.2](https://github.com/obstudio/ThuliumMusicPlayer-WL/blob/master/changes/2.2.md) (2018.4.21)
- [2.1](https://github.com/obstudio/ThuliumMusicPlayer-WL/blob/master/changes/2.1.md) (2018.4.1)

