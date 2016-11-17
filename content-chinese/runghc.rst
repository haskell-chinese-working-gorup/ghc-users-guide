.. _runghc:

使用 runghc
============

.. index::
   single: runghc

``runghc`` 允许你在不编译的情况下运行 Haskell 程序。

.. _runghc-introduction:

使用方法
--------

``runghc`` 的命令行格式类似于:

::

    runghc [runghc 标志] [GHC 标志] module [程序参数]

任何不被 runghc 识别的标志都会自动被传递给 GHC。
如果一个标志可以同时被 runghc 与 GHC 识别，但是想把它传递给 GHC，可以在标志
前加上 ``--`` 分隔符。分隔符后面的标志会被当作仅被 GHC 使用的参数。另外也可以
使用 runghc 的选项 ``--ghc-arg=<arg>`` 来直接传递标志或者参数给 GHC。

``module`` 可以是 Haskell 源文件名，无论该文件有没有扩展名。
如果文件名以 ``-`` 开头，那么可以使用第二个 ``--`` 来指示标志位结束。任何在
第二个 ``--`` 后面的内容都会被当做程序文件名/模块名及它们的参数。比如:

- ``runghc -- -- -hello.hs``

runghc 标志
------------

runghc 接受下列参数:

- ``-f /path/to/ghc``: 提供给 runghc 执行程序所使用的 GHC 的路径。runghc 默认在系统路径中搜索 GHC。
- ``--ghc-arg=<arg>``: 传递一个选项或者参数给 GHC
- ``--help``: 打印使用方法
- ``--version``: 打印版本信息

GHC 标志
---------

正如前面讨论的，必要时可以使用 ``--`` 或者 ``--ghc-arg=<arg>`` 指明是否是传递
给 GHC 的标志位。比如 ``-f`` 会被 runghc 识别，因此要传递 ``-fliberate-case`` 
给 GHC 可以使用下面任何一种方式:

- ``runghc -- -fliberate-case``
- ``runghc --ghc-arg=-fliberate-case``

注意任何非标志参数都不会传递给 GHC。未使用的非标志参数会被认为是要执行的程序
名字。如果一个 GHC 标志有参数，使用 ``--ghc-arg=<arg>`` 传递参数给 GHC。
例如，如果想传递 ``-package foo`` 给 GHC，可以使用下列形式:

- ``runghc -package --ghc-arg=foo Main.hs``
- ``runghc --ghc-arg=-package --ghc-arg=foo Main.hs``
