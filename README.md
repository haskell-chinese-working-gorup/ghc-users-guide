GHC用户指南中文版
=================

英文原版clone自[GHC官方](https://github.com/bgamari/ghc-users-guide)，以方便通过pr来同步，操作指南：

+ 不要改动`content`, `images`, `export.sh`, `ghc_config.py`，@winterland 负责定时pr变更到这些目录和文件。

+ 中文翻译在`content-chinese`目录下，也是本repo的主要工作目录，目前存放着未翻译的源码，各位直接打开对应文件翻译即可。

+ 和原文档一致，中文翻译使用[reStructuredText](https://en.wikipedia.org/wiki/ReStructuredText)。

如需本地预览请安装[Sphinx](http://www.sphinx-doc.org)：

```
pip install Sphinx
pip install sphinx_rtd_theme
sphinx-build -b html content-chinese build
```

然后你就可以打开`build`目录下的`index.html`来预览你的成果了！另外`build`已经被加入了`.gitignore`，所以也不需要手动清理。根据你的操作系统配置不同，你可能需要设置一些环境变量：

```
export LANG=en_US.UTF-8  
export LC_ALL=en_US.UTF-8  
```

目标和进度
---------

我们希望能够在GHC8.2发版（明年3～4月份）之前，完成本手册的翻译，目前各个文件的翻译任务分配如下：

```
+ license               0% (暂定不需要翻译)
+ intro                 0%  @Balter Notz
+ 8.2.1-notes           0%  @Balter Notz 
+ ghci                  25%  @kdepp       -- THIS IS A HARD ONE!
+ runghc                0%  @Bo Yang 
+ usage                 0% 
    + using                    0% @Bo Yang
    + using-warnings           0% @B1nj0y
    + using-optimisation       0% @B1nj0y
    + using-concurrent         0% @HaotianXue
    + flags                    0% @HaotianXue
    + runtime_control          0% @Yao Lu 
    + separate_compilation     0% @Yao Lu 
    + packages                 0% @winterland
    + codegens                 0% @winterland 
    + phases                   0% @winterland
    + shared_libs              0% @sunyx2016 
    + debugging                0% @sunyx2016
+ profiling             0%  
+ sooner                0%
+ lang                  0%
+ ffi-chap              0%
+ extending_ghc         0%
+ gone_wrong            0%
+ debug-info            0%
+ utils                 0%
+ win32-dlls            0%
+ bugs                  0%
+ eventlog-formats      0%
+ editing-guide         100% @winterland
```

Kudos！
------

让我们向这些勇敢的同志们致敬！

+ [Balter Notz](https://github.com/orgs/haskell-chinese-working-group/people/BalterNotz)
+ [Bo Yang](https://github.com/orgs/haskell-chinese-working-group/people/ggarlic)
+ [B1nj0y](https://github.com/orgs/haskell-chinese-working-group/people/gingerhot)
+ [HaotianXue](https://github.com/orgs/haskell-chinese-working-group/people/HaotianXue)
+ [kdepp](https://github.com/orgs/haskell-chinese-working-group/people/kdepp)
+ [Yao Lu](https://github.com/orgs/haskell-chinese-working-group/people/ruujoa)
+ [sunyx2016](https://github.com/sunyx2016)
