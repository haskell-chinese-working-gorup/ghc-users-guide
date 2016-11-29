.. _introduction-GHC:

GHC介绍
===================

本文是格拉斯哥大学 Haskell 编译器（GHC）的使用指南，对应的 Haskell 语言
版本是 `Haskell 2010 <http://www.haskell.org/>`__ ，GHC 是一个交互式的
和批编译系统。

GHC 由两个主要部分组成：一个是交互式的解译器（GHCi，参见 :ref:`ghci` ）
另一个是批编译器（参见 :ref:`using-ghc` ）。事实上，GHC 是一个单一的的
程序，只是以两种不同的选项运行，以提供交互功能或批编译功能。

批编译器可以与 GHCi 一起使用：编译过的模块能够被加载到交互环境中并能够像
解释的代码那样被使用，事实上当你使用 GHCi 时，大部分的库都将被预编译，这
意味着你可以同时享受预编译的代码带来的执行速度上的提升及开发过程中快速编
译所写代码而带来的便利性。

GHC 支持多种扩展特性，包括：并发、外部函数接口、异常、类型系统扩展（如：
多参数类型类、todo）
GHC supports numerous language extensions, including concurrency, a
foreign function interface, exceptions, type system extensions such as
multi-parameter type classes, local universal and existential
quantification, functional dependencies, scoped type variables and
explicit unboxed types. These are all described in
:ref:`ghc-language-features`.

GHC 提供了全面的优化措施可供选择，所以当你真的使用了这些优化措施时，GHC
能够生产出执行速度相当快的代码。GHC 默认选择以最快的速度编译代码，而不是
对生成的代码做太多的优化。（todo）
GHC has a comprehensive optimiser, so when you want to Really Go For It
(and you've got time to spare) GHC can produce pretty fast code.
Alternatively, the default option is to compile as fast as possible
while not making too much effort to optimise the generated code
(although GHC probably isn't what you'd describe as a fast compiler :-).

GHC 的性能分析系统提供“成本中心堆栈”的方式：以调用图的形式显示 Haskell 
程序的运行情况。参见 :ref:`profiling` 。

GHC 同时提供了许多库，它们将在以后的章节中分别进行介绍。

.. _getting:

获得GHC
-------------

从 `GHC主页<http://www.haskell.org/ghc/>`__ 的"download"链接下载适合你的
系统的 GHC 发布包。

如果你想自己编译 GHC，请阅读 :ghc-wiki:`GHC Building Guide <Building>` 
了解如何下载 GHC 源代码及如何在你的系统上编译GHC，值得注意的是，GHC 本身
是用 Haskell 语言编写的，所以你需要预先安装 GHC 以便编译 GHC 源代码。

.. _mailing-lists-GHC:

线上资源：网站，邮件列表，等等。
------------------------------------------------

.. index::
   single: mailing lists, Glasgow Haskell
   single: Glasgow Haskell mailing lists

在网络上有以下几个链接你可能会感兴趣：

- `GHC 主页 <http://www.haskell.org/ghc/>`__

- `GHC 开发者主页 <http://ghc.haskell.org/trac/ghc/>`__ （开发者文档、
wiki，缺陷追踪）

我们使用下面的邮件列表进行 GHC 相关的讨论，如果你觉得合适，我们鼓励你加
入进去。

``glasgow-haskell-users``
    这个邮件列表用于 GHC 用户之间的通信，如果你有关于 GHC 的问题，请首
    先查阅 `FAQ <http://wwww.haskell.org/haskellwiki/GHC/FAQ>`__ 。

    订阅者能够通过向glasgow-haskell-users@haskell.org发送邮件的方式与该
    邮件列表内的用户进行交流。更多的信息请参见：
    `Mailman page <http://www.haskell.org/mailman/listinfo/glasgow-haskell-users>`__ 。

``ghc-devs``
    该邮件列表用于 GHC 开发者之间的交流，如果你有使用 GHC API 或者对 GHC 
    的实现有任何的问题，请随时通过该邮件列表进行讨论。

    订阅者可以通过向ghc-devs@haskell.org发送邮件的形式与该列表内的人员
    进行交流。更多的信息请参见：
    `Mailman page <http://www.haskell.org/mailman/listinfo/ghc-devs>`__ 。

``www.haskell.org`` 还有提供一些其它的 Haskell 和 GHC 相关的邮件列表，
请到 http://www.haskell.org/mailman/listinfo/ 页面查看完整的邮件列表。

.. _bug-reporting:

报告 GHC 中的 bug
---------------------

.. index::
   single: bugs; reporting
   single: reporting bugs

GHC 是一个快速开发中的系统，所以 GHC 中一定会藏有 bug，如果您在GHC 中发
现了一个 bug，请根据 :ghc-wiki:`this wiki page <ReportABug>` 中的信息上
报该 bug。

.. _version-numbering:

GHC 版本号编码规则
----------------------------

.. index::
   single: version, of ghc

以 GHC 6.8 版为例，我们使用了以下的版本号编码规则：

    稳定的分支以 ``x.y`` 的形式命名，其中 y 是偶数。在稳定的 ``x.y`` 分
    支上进行的发布以 ``x.y.z`` 的形式对其版本号进行命名，其中 z 是次要
    版本号，并且 z >= 1。次要版本号只用于 bug 修复，并不改变任务系统级
    的程序接口。然尔，如果你在旧版的 GHC 上安装了新的 bug 修复版，那么
    你需要重新编译那些使用旧版的库编译的代码。

    以一个稳定发布版的版本号 ``x.y.z`` 为例， ``__GLASGOW_HASKELL__`` 
    （参见 :ref:`c-pre-processor` ）是一个 <xyy> 形式的整数（如果 y
    只有一位数字，则将在 y 的前面插入一个 0 ，以 GHC 的 6.8.2 版为例
    你将会得到 ``__GLASGOW_HASKELL__==608`` ）。

    .. index::
       single: __GLASGOW_HASKELL__

    我们也会在现有的稳定分支创建 snapshot 版本
    `从这里获取 <http://www.haskell.org/ghc/dist/stable/dist/>`__ ，最
    新的源代码可以从 :ghc-wiki:`git仓库 <Repositories>` 获取。

    稳定分支的 snapshot 版版本号以 ``x.y.z.YYYYMMDD`` 的形式命名，其中的
    ``YYYYMMDD`` 是该版本创建时的日期，并且该分支的下一个发布版的版本号
    将以 ``x.y.z+1`` 的形式命名。比如， ``6.8.1.20040225`` 是 ``6.8`` 
    分支开发 ``6.8.2`` 版时的 snapshot 版本号。

    snapshot 版的 ``__GLASGOW_HASKELL__`` 的值依然是整形 <xyy>，你不应该
    写任何的条件测试该值，但，由于接口其本上每天都在变动，并且我们也没有
    更细粒度的设置 ``__GLASGOW_HASKELL__`` 的值，因此，你仅需要使用对
    ``__GLASGOW_HASKELL__`` 的测试结果，根据其值是等于、大于或者小于一个
    主版本进行条件编译即可。

    我们会创建最新的 snapshot 发布版 `从该处下载
    <http://www.haskell.org/ghc/dist/current/dist/>`__ ，最新的源代码从
    :ghc-wiki:`git仓库 <Repositories>` 获取。

    不稳定分支的 snapshot 发布版以 ``x.y.YYYYMMDD`` 的形式命名，其中
    ``YYYYMMDD`` 是该 snapshot 版源代码创建的日期。如， ``6.7.20040225``
    是在 ``6.8`` 分支创建之前最新的发布分支。

你本地安装的 GHC 的版本号可以通过 ``ghc --version`` 命令查看（详情请参阅
 :ref:`options-help` ）。

编译器的版本能够在编译的代码里使用 ``MIN_VERSION_GLASGOW_HASKELL`` CPP宏
进行测试（只有在使用 :ghc-flag:`-XCPP` 选项时能定义）。更多详情请参阅
:ref:`standard-cpp-macros`

The compiler version can be tested within compiled code with the
``MIN_VERSION_GLASGOW_HASKELL`` CPP macro (defined only when
:ghc-flag:`-XCPP` is used). See :ref:`standard-cpp-macros` for details. 

