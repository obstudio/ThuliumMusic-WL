## Thulium Music Player v2.1

#### 外观
- 完美适配Windows，macOS，Linux等主流操作系统
- 加入了工作簿功能，允许用户导入和导出调试记录

#### 语法
- 头音轨中新增识别符`!`，用于控制其后的函数仅对当前乐章生效
- 在头音轨中加入了对多种反复的支持：
  - Coda/ToCoda(`+`)
  - Segno(`s`)
  - DaSegno(`DS`)
  - DaCapo(`DC`)
  - Fine(`Fine`)
- `ConOct`函数更名为`Con`函数
- `Oct`和`Vol`函数支持读入数组
- `KeyOct`函数的功能合并入`Key`函数
- 新增了`KeyShift`函数，用于实现升降调
- 函数简记法系统：加入了`^`识别符以实现无符号的效果

#### 新增：文档系统
- 加入了文档功能，用户可以查看各种语法，各种函数的使用方法
- 第一批文档内容全部完成，包括下面的页面：

> 函数：Key, Oct, Con, KeyShift, Spd, Dur, Stac, BarBeat, Vol, Acct, Light, Trace, Tuplet, Fermata, Ferm, Tremolo1, Tremolo2, Portamento, Port, GraceNote, Appoggiatura, Arpeggio, Seg, FadeIn, FadeOut。

#### 歌曲

#### 修复

