为 GHC 用户指南贡献一份力量
=========================

GHC 用户指南是格拉斯哥 Haskell 编译器（Glasgow Haskell Compiler）的主要参考文档。不仅如此，它常常也被用来作为 Haskell 的语言规范（虽然有很多争议），是很多语言扩展的唯一非学术性参考。

从 GHC 8.0 开始，用户指南使用 `ReStructuredText <https://en.wikipedia.org/wiki/ReStructuredText>`__ (或者简称 ReST, RST )，一门灵活轻量专门用于生成文档的标记语言编写， `Sphinx <http://sphinx-doc.org/>`__ 工具被用来生成最终的 PDF 和 HTML 文档。

本篇文档（也是使用 ReST 编写）作为一个简单的 ReST 介绍，用来说明用户指南里常用的一些约定，而不是一份详尽的 ReST 指南。需要 ReST 指南的朋友可以参考 `这里 <#references>`__.

基础
----

文档中允许 Unicode 字符。

大部分语法和你的预期是一致的。例如，

.. code-block:: rest

    This is a paragraph containing a few sentences of text. Purple turtles walk
    through green fields of lofty maize. Lorem ipsum dolor sit amet, consectetur
    adipiscing elit. Some lists,

    这是一个由几句话构成的段落，紫色的乌龟穿过种满高高的玉米的绿色田野。Lorem ipsum dolor sit amet, consectetur
    adipiscing elit. 一些列表，

    1. 这是一个列表项

       a. 接着一个次级列表项
       b. 然后是另一个！
       c. 然后跟上 ``一小段代码`` 和一些 *强调*.

    2. 回到外层列表

    或者你更喜欢使用无序列表，

    * Foo
    * Fizzle

      - Bar
      - Blah

    又或者是一个定义表,

    *Chelonii*
        The taxonomic order consisting of modern turtles
    *Meiolaniidae*
        The taxonomic order of an extinct variety of herbivorous turtles.

注意列表项和次级列表项之间的空行，同一层级的列表项应该保持相同的缩减。
同时注意在无序列表的符号和有序列表的数字前都不应该由空白字符（以免列表被意外缩进）。

上面的段落渲染效果如下,

    This is a paragraph containing a few sentences of text. Purple turtles walk
    through green fields of lofty maize. Lorem ipsum dolor sit amet, consectetur
    adipiscing elit. Some lists,

    这是一个由几句话构成的段落，紫色的乌龟穿过种满高高的玉米的绿色田野。Lorem ipsum dolor sit amet, consectetur
    adipiscing elit. 一些列表，

    1. 这是一个列表项

       a. 接着一个次级列表项
       b. 然后是另一个！
       c. 然后跟上 ``一小段代码`` 和一些 *强调*.

    2. 回到外层列表

    或者你更喜欢使用清单，

    * Foo
    * Fizzle

      - Bar
      - Blah

    又或者是一个定义表,

    *Chelonii*
        The taxonomic order consisting of modern turtles
    *Meiolaniidae*
        The taxonomic order of an extinct variety of herbivorous turtles.


标题
~~~~

虽然 Rest 支持很多种标题的格式，但是本文档中的约定如下，

.. code-block:: rest

    1级标题
    ==============

    2级标题
    --------------

    3级标题
    ~~~~~~~~~~~~~~

    4级标题
    ^^^^^^^^^^^^^^


代码格式
~~~~~~~~

Haskell
^^^^^^^

代码片段可以按照行或者段落为单位添加，嵌在行内的代码可以用``包围来标识，
代码段落则需要在上一个段落结束之后使用::开始，同时代码本身需要缩进。

.. code-block:: rest

    ``fib`` 函数可以这样定义, ::

        fib :: Integer -> Integer
        fib 1 = 1
        fib n = n * fib (n - 1)

上面的段落会被渲染成，

    ``fib`` 函数可以这样定义, ::

        fib :: Integer -> Integer
        fib 1 = 1
        fib n = n * fib (n - 1)

其他语言
^^^^^^^^

双冒号标记的段落默认会按照 Haskell 代码进行高亮处理。如果你需要标记其他语言请使用
``.. code-block`` `指令
<http://sphinx-doc.org/markup/code.html#directive-code-block>`__ 并显式指定语言。

.. code-block:: rest

    这是一段简单的脚本,

    .. code-block:: sh

        #!/bin/bash
        echo "Hello World!"


超链接
~~~~~~

用户指南内部链接
^^^^^^^^^^^^^^^^

经常我们需要给文档的一个小节命名，以便于在其他位置添加对其的引用，

.. code-block:: rest

    .. _options-platform:

    平台相关标志
    ------------

    有很多平台相关的标志。

    某个其他的章节
    --------------

    GHC 支持很多 :ref:`x86 特定的功能 <options-platform>`。

    请参考 :ref:`options-platform` 获取更多细节。


指向GHC Trac的链接
^^^^^^^^^^^^^^^^^^

有一些特定的宏（macro）用来方便地插入指向 GHC Trac Wiki 上面的文章和工单。

.. code-block:: rest

    请参考 :ghc-wiki:`Commentary/Latedmd` 了解关于需求分析的细节。

    请参考 :ghc-wiki:`coding style <Commentary/CodingStyle>` 获取指南。

    请参考 :ghc-ticket:`123` 了解更多相关讨论。

    请参考 :ghc-ticket:`this bug <123>` 了解失败时究竟发生了什么。


指向外部资源的链接
^^^^^^^^^^^^^^^^^^

外部链接可以使用一下几种方式书写，

.. code-block:: rest

    请参考 `GHC Wiki <http://ghc.haskell.org/wiki>`_ 获取更多细节。

    请参考 `GHC Wiki`_ 获取更多细节。

    .. _GHC Wiki: http://ghc.haskell.org/wiki


指向核心库的 Haddock 文档
^^^^^^^^^^^^^^^^^^^^^^^^^

你会经常需要添加指向伴随 GHC 发行的核心库文档的链接。用户指南的构建系统提供
引用核心库文档的命令，

* ``base``: ``:base-ref:``
* ``cabal``: ``:cabal-ref:``
* ``ghc-prim``: ``:ghc-prim-ref:``

举个例子，

.. code-block:: rest

    请参考文档 :base-ref:`Control.Applicative <Control-Applicative.html>`
    获取更多细节。


添加索引
~~~~~~~~~~

索引可以在文档的任意位置引用所在段落，它们看上去是这样的，
    
.. code-block:: rest

    这里是关于Strict Haskell扩展的一些讨论。

    .. index::
        single: strict haskell
        single: 语言扩展; StrictData

上面的例子里我们创建了指向 "Strict Haskell" 的索引，一个是简单的 "strict haskell" 标题索引，
另一个则是在"语言扩展"下的 "StrictData" 次级标题索引。

遗憾的是你不可以在索引标题中添加行内元素 (例如行内代码)。 


引用文献
--------

引用文献可以按照如下方式标记,

.. code-block:: rest

    参考原论文 [Doe2008]_

    .. [Doe2008] John Doe and Leslie Conway.
                 "This is the title of our paper" (2008)


警告（Admonitions）
------------------

`Admonitions`_ 是用来引起读者注意的段落，它们可以有效的把段落从上下文中凸显出来从而达到吸引注意力的作用，
但是出于可读性的考虑你也不应该过分依赖它们。

.. code-block:: rest

    .. important::

        请友好地对待贡献者们，并给予力所能及的支持。

会被渲染成,

    .. important::

        请友好地对待贡献者们，并给予力所能及的支持。

以下是几种警告的类型，

.. hlist::
    :columns: 3

    * attention
    * caution
    * danger
    * error
    * hint
    * important
    * note
    * tip
    * warning


.. _Admonitions: http://docutils.sourceforge.net/docs/ref/rst/directives.html#admonitions

给命令行参数和 GHCi 命令书写文档
--------------------------------

:file:`conf.py` 定义了一些 Sphinx 对象类型用于书写 GHCi 命令（``ghci-cmd``）, :program:`ghc` 
命令行参数（``ghc-flag``），以及运行时参数（``rts-flag``）。

命令行参数
~~~~~~~~~~

``ghc-flag`` 和 ``rts-flag`` 指令分别用来书写 :program:`ghc` 可执行文件和运行时的命令行参数的文档，
举个例子，

.. code-block:: rest

    .. rts-flag:: -C <seconds>

       :default: 20 milliseconds

       Sets the context switch interval to ⟨s⟩ seconds.

会被渲染为,

    .. rts-flag:: -C <seconds>

       :default: 20 milliseconds

       Sets the context switch interval to ⟨s⟩ seconds.

同时会自动生成对应的索引项。

GHCi 命令
~~~~~~~~~

``ghci-cmd`` 指令被用来给 GHCi 命令添加文档. 例如, 我们可以这样来说明 GHCi 的 ``:module`` 命令,

.. code-block:: rest

    .. ghci-cmd:: :module [*] <file>

        Load a module

会被渲染为，

    .. ghci-cmd:: :module [*] <file>

        Load a module

之后就可以使用命令 ``:module`` 来引用它了，

.. code-block:: rest

    The GHCi :ghci-cmd:`:load` and :ghci-cmd:`:module` commands are used
    to modify the modules in scope.

和命令行参数一样，GHCi 命令也会自动生成对应的索引项。
    
样式约定
--------

当说明用户命令时，一个常见的需求是标记可自定义的分词位置，在本文档中我们使用约定 
``⟨subst⟩`` 来表示（注意这些是三角括号 ``U+27E8`` 和 ``U+27E9`` ，而不是小于／大于号）。

.. important::

    出于视觉上的协调，本文档约定在中英文之间应该增加半角空格。

.. _references:

GHC 命令行参数参考
------------------

GHC 命令行选项和参数表格（:file:`flags.rst` ）不适合使用 ReST 来书写，因此它是通过 :file:`utils/mkUserGuidePart` 自动生成的。任何加入 GHC 的命令行选项都应同时被加入 :file:`utils/mkUserGuidePart/Options` 的对应文件中。

ReST 参考材料
-------------

* `Sphinx ReST Primer`_: 一个很好的开始。
* `Sphinx extensions`_: Sphinx 是如何扩展 ReST 的。
* `ReST reference`_: 当你需要更多细节时。
* `Directives reference`_: 指令参考。

.. _Sphinx ReST Primer: http://sphinx-doc.org/rest.html
.. _ReST reference: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
.. _Sphinx extensions: http://sphinx-doc.org/markup/index.html
.. _Directives reference: http://docutils.sourceforge.net/docs/ref/rst/directives.html#code
