为GHC用户指南贡献一份力量
=========================

GHC用户指南是格拉斯哥Haskell编译器（Glasgow Haskell Compiler）的主要参考文档。不仅如此，它常常也被用来作为Haskell的语言规范（虽然有很多争议），是很多语言扩展的唯一非学术性参考。

从GHC 8.0开始，用户指南使用 `ReStructuredText <https://en.wikipedia.org/wiki/ReStructuredText>`__ (或者简称ReST、RST)，一门灵活轻量专门用于生成文档的标记语言编写， `Sphinx <http://sphinx-doc.org/>`__ 工具被用来生成最终的PDF和HTML文档。

本篇文档（也是使用ReST编写）作为一个简单的ReST介绍，用来说明用户指南里常用的一些约定，而不是一份详尽的ReST指南。需要ReST指南的朋友可以参考 `这里 <#references>`__.

基础
----

文档中允许Unicode字符。

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

虽然Rest支持很多种标题的格式，但是本文档中的约定如下，

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

双冒号标记的段落默认会按照Haskell代码进行高亮处理。如果你需要标记其他语言请使用
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

    GHC支持很多 :ref:`x86 特定的功能 <options-platform>`。

    请参考 :ref:`options-platform` 获取更多细节。


指向GHC Trac的链接
^^^^^^^^^^^^^^^^^^

有一些特定的宏（macro）用来方便地插入指向GHC Trac Wiki上面的文章和工单。

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


指向核心库的Haddock文档
^^^^^^^^^^^^^^^^^^^^^^^

你会经常需要添加指向伴随GHC发行的核心库文档的链接。用户指南的构建系统提供
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
        single: language extensions; StrictData

上面的例子里我们创建了指向"Strict Haskell"的索引，一个是简单的"strict haskell"标题，
另一个则是在"language extensions"下的"StrictData"次级标题。

遗憾的是你不可以在索引标题中添加行内元素 (例如行内代码)。 


引用文献
--------

引用文献可以按照如下方式标记,

.. code-block:: rest

    参考原论文 [Doe2008]_

    .. [Doe2008] John Doe and Leslie Conway.
                 "This is the title of our paper" (2008)


Admonitions
-----------

`Admonitions`_ are block elements used to draw the readers attention to a point.
They should not be over-used for the sake of readability but they can be quite
effective in separating and drawing attention to points of importance,

.. code-block:: rest

    .. important::

        Be friendly and supportive to your fellow contributors.

Would be rendered as,

    .. important::

        Be friendly and supportive to your fellow contributors.

There are a number of admonitions types,

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

Documenting command-line options and GHCi commands
--------------------------------------------------

:file:`conf.py` defines a few Sphinx object types for GHCi commands
(``ghci-cmd``), :program:`ghc` command-line options (``ghc-flag``), and runtime
:system options (``rts-flag``),

Command-line options
~~~~~~~~~~~~~~~~~~~~

The ``ghc-flag`` and ``rts-flag`` roles/directives can be used to document
command-line arguments to the :program:`ghc` executable and runtime system,
respectively. For instance,

.. code-block:: rest

    .. rts-flag:: -C <seconds>

       :default: 20 milliseconds

       Sets the context switch interval to ⟨s⟩ seconds.

Will be rendered as,

    .. rts-flag:: -C <seconds>

       :default: 20 milliseconds

       Sets the context switch interval to ⟨s⟩ seconds.

and will have an associated index entry generated automatically.

GHCi commands
~~~~~~~~~~~~~

The ``ghci-cmd`` role and directive can be used to document GHCi directives. For
instance, we can describe the GHCi ``:module`` command,

.. code-block:: rest

    .. ghci-cmd:: :module [*] <file>

        Load a module

which will be rendered as,

    .. ghci-cmd:: :module [*] <file>

        Load a module

And later refer to it by just the command name, ``:module``,

.. code-block:: rest

    The GHCi :ghci-cmd:`:load` and :ghci-cmd:`:module` commands are used
    to modify the modules in scope.

Like command-line options, GHCi commands will have associated index entries
generated automatically.

Style Conventions
-----------------

When describing user commands and the like it is common to need to denote
user-substitutable tokens. In this document we use the convention, ``⟨subst⟩``
(note that these are angle brackets, ``U+27E8`` and ``U+27E9``, not
less-than/greater-than signs).


.. _references:

GHC command-line options reference
----------------------------------

The tabular nature of GHC flags reference (:file:`flags.rst`) makes it very
difficult to maintain as ReST. For this reason it is generated by
:file:`utils/mkUserGuidePart`. Any command-line options added to GHC should
be added to the appropriate file in :file:`utils/mkUserGuidePart/Options`.


ReST reference materials
------------------------

* `Sphinx ReST Primer`_: A great place to start.
* `Sphinx extensions`_: How Sphinx extends ReST
* `ReST reference`_: When you really need the details.
* `Directives reference`_

.. _Sphinx ReST Primer: http://sphinx-doc.org/rest.html
.. _ReST reference: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
.. _Sphinx extensions: http://sphinx-doc.org/markup/index.html
.. _Directives reference: http://docutils.sourceforge.net/docs/ref/rst/directives.html#code
