为GHC用户指南贡献一份力量
=========================

GHC用户指南是格拉斯哥Haskell编译器（Glasgow Haskell Compiler）的主要参考文档。不仅如此，它常常也被用来作为Haskell的语言规范（虽然有很多争议），是很多语言扩展的唯一非学术性参考。

从GHC 8.0开始，用户指南使用 `ReStructuredText <https://en.wikipedia.org/wiki/ReStructuredText>`__ (或者简称ReST、RST)，一门灵活轻量专门用于生成文档的标记语言编写， `Sphinx <http://sphinx-doc.org/>`__ 工具被用来生成最终的PDF和HTML文档。

本篇文档（也是使用ReST编写）作为一个简单的ReST介绍，用来说明用户指南里常用的一些约定，而不是一份详尽的ReST指南。需要ReST指南的朋友可以参考 `这里 <#references>`__.

基础
----

文档中允许Unicode字符。

大部分语法和你的预期是一致的。例如：

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


Headings
~~~~~~~~

While ReST can accommodate a wide range of heading styles, we have standardized
on this convention in the User's Guide,

.. code-block:: rest

    Header level 1
    ==============

    Header level 2
    --------------

    Header level 3
    ~~~~~~~~~~~~~~

    Header level 4
    ^^^^^^^^^^^^^^


Formatting code
~~~~~~~~~~~~~~~

Haskell
^^^^^^^

Code snippets can be included as both inline and block elements. Inline
code is denoted with double-backticks whereas block of code are introduced
by ending a paragraph with double-colons and indentation,

.. code-block:: rest

    The ``fib`` function is defined as, ::

        fib :: Integer -> Integer
        fib 1 = 1
        fib n = n * fib (n - 1)

Which would be rendered as,

    The ``fib`` function is defined as, ::

        fib :: Integer -> Integer
        fib 1 = 1
        fib n = n * fib (n - 1)

Other languages
^^^^^^^^^^^^^^^

Double-colon blocks are syntax-highlighted as Haskell by default. To avoid this
use a
``.. code-block`` `directive
<http://sphinx-doc.org/markup/code.html#directive-code-block>`__ with explicit
language designation,

.. code-block:: rest

    This is a simple shell script,

    .. code-block:: sh

        #!/bin/bash
        echo "Hello World!"


Links
~~~~~

Within the Users Guide
^^^^^^^^^^^^^^^^^^^^^^

Frequently we want to give a name to a section so it can be referred to
from other points in the document,

.. code-block:: rest

    .. _options-platform:

    Platform-specific Flags
    -----------------------

    There are lots of platform-specific flags.

    Some other section
    -------------------

    GHC supports a variety of :ref:`x86 specific features <options-platform>`.

    See :ref:`options-platform` for details.


To GHC Trac resources
^^^^^^^^^^^^^^^^^^^^^

There are special macros for conveniently linking to GHC Trac
Wiki articles and tickets,

.. code-block:: rest

    See :ghc-wiki:`Commentary/Latedmd` for details on demand analysis.

    See the :ghc-wiki:`coding style <Commentary/CodingStyle>` for guidelines.

    See the :ghc-ticket:`123` for further discussion.

    See the :ghc-ticket:`this bug <123>` for what happens when this fails.


To external resources
^^^^^^^^^^^^^^^^^^^^^

External links can be written in either of these ways,

.. code-block:: rest

    See the `GHC Wiki <http://ghc.haskell.org/wiki>`_ for details.

    See the `GHC Wiki`_ for details.

    .. _GHC Wiki: http://ghc.haskell.org/wiki


To core library Haddock documentation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is often useful to be able to refer to the Haddock documention of the
libraries shipped with GHC. The users guide's build system provides
commands for referring to documentation for the following core GHC packages,

* ``base``: ``:base-ref:``
* ``cabal``: ``:cabal-ref:``
* ``ghc-prim``: ``:ghc-prim-ref:``

For instance,

.. code-block:: rest

    See the documentation for :base-ref:`Control.Applicative <Control-Applicative.html>`
    for details.


Index entries
~~~~~~~~~~~~~

Index entries can be included anywhere in the document as a block element.
They look like,
    
.. code-block:: rest

    Here is some discussion on the Strict Haskell extension.

    .. index::
        single: strict haskell
        single: language extensions; StrictData

This would produce two entries in the index referring to the "Strict Haskell"
section. One would be a simple "strict haskell" heading whereas the other would
be a "StrictData" subheading under "language extensions".

Sadly it is not possible to use inline elements (e.g. monotype inlines) inside
index headings.

Citations
---------

Citations can be marked-up like this,

.. code-block:: rest

    See the original paper [Doe2008]_

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
