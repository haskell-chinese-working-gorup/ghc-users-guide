.. _introduction-GHC:

GHC介绍
===================

本文是格拉斯哥大学 Haskell 编译器（GHC）的使用指南，对应的 Haskell 语言
版本是 `Haskell 2010 <http://www.haskell.org/>`__ ，GHC 是一个交互式的
批编译系统。
This is a guide to using the Glasgow Haskell Compiler (GHC): an
interactive and batch compilation system for the
`Haskell 2010 <http://www.haskell.org/>`__ language.

GHC 由两个主要部分组成：一个是交互式的解译器（GHCi，参见 :ref:`ghci` ）
另一个是批编译器（参见 :ref:`using-ghc` ）。事实上，GHC 是一个单一的的
程序，只是以两种不同的选项运行，以提供交互功能或批编译功能。
GHC has two main components: an interactive Haskell interpreter (also
known as GHCi), described in :ref:`ghci`, and a batch compiler,
described throughout :ref:`using-ghc`. In fact, GHC consists of a single
program which is just run with different options to provide either the
interactive or the batch system.

批编译器可以与 GHCi 一起使用：编译过的模块能够被加载到交互环境中并能够像
解释的代码那样被使用，事实上当你使用 GHCi 时，大部分的库都将被预编译，这
意味着你可以同时享受预编译的代码带来的执行速度上的提升及开发过程中快速编
译所写代码而带来的便利性。
The batch compiler can be used alongside GHCi: compiled modules can be
loaded into an interactive session and used in the same way as
interpreted code, and in fact when using GHCi most of the library code
will be pre-compiled. This means you get the best of both worlds: fast
pre-compiled library code, and fast compile turnaround for the parts of
your program being actively developed.

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
GHC's profiling system supports "cost centre stacks": a way of seeing
the profile of a Haskell program in a call-graph like structure. See
:ref:`profiling` for more details.

GHC 同时提供了许多库，它们将在以后的章节中分别进行介绍。
GHC comes with a number of libraries. These are described in separate
documentation.

.. _getting:

获得GHC
-------------

从 `GHC主页<http://www.haskell.org/ghc/>`__ 的"download"链接下载适合你的
系统的 GHC 发布包。
Go to the `GHC home page <http://www.haskell.org/ghc/>`__ and follow the
"download" link to download GHC for your platform.

如果你想自己编译 GHC，请阅读 :ghc-wiki:`GHC Building Guide <Building>` 
了解如何下载 GHC 源代码及如何在你的系统上编译GHC，值得注意的是，GHC 本身
是用 Haskell 语言编写的，所以你需要预先安装 GHC 以便编译 GHC 源代码。
Alternatively, if you want to build GHC yourself, head on over to the
:ghc-wiki:`GHC Building Guide <Building>` to find out how to get the sources,
and build it on your system. Note that GHC itself is written in Haskell, so you
will still need to install GHC in order to build it.

.. _mailing-lists-GHC:

线上资源：网站，邮件列表，等等。
------------------------------------------------

.. index::
   single: mailing lists, Glasgow Haskell
   single: Glasgow Haskell mailing lists

在网络上有以下几个链接你可能会感兴趣：
On the World-Wide Web, there are several URLs of likely interest:

- `GHC 主页 <http://www.haskell.org/ghc/>`__
-  `GHC home page <http://www.haskell.org/ghc/>`__

- `GHC 开发者主页 <http://ghc.haskell.org/trac/ghc/>`__ （开发者文档、
wiki，缺陷追踪）
-  `GHC Developers Home <http://ghc.haskell.org/trac/ghc/>`__ (developer
   documentation, wiki, and bug tracker)

我们使用下面的邮件列表进行 GHC 相关的讨论，如果你觉得合适，我们鼓励你加
入进去。
We run the following mailing lists about GHC. We encourage you to join,
as you feel is appropriate.

``glasgow-haskell-users``
    这个邮件列表用于 GHC 用户之间的通信，如果你有关于 GHC 的问题，请首
    先查阅 `FAQ <http://wwww.haskell.org/haskellwiki/GHC/FAQ>`__ 。
    This list is for GHC users to chat among themselves. If you have a
    specific question about GHC, please check the
    `FAQ <http://www.haskell.org/haskellwiki/GHC/FAQ>`__ first.

    订阅者能够通过向glasgow-haskell-users@haskell.org发送邮件的方式与该
    邮件列表内的用户进行交流。更多的信息请参见：
    `Mailman page <http://www.haskell.org/mailman/listinfo/glasgow-haskell-users>`__ 。
    Subscribers can post to the list by sending their message to 
    glasgow-haskell-users@haskell.org. Further information can be found
    on the
    `Mailman page <http://www.haskell.org/mailman/listinfo/glasgow-haskell-users>`__.

``ghc-devs``
    该邮件列表用于 GHC 开发者之间的交流，如果你有使用 GHC API 或者对 GHC 
    的实现有任何的问题，请随时通过该邮件列表进行讨论。
    The GHC developers hang out here. If you are working with the GHC API
    or have a question about GHC's implementation, feel free to chime in.

    订阅者可以通过向ghc-devs@haskell.org发送邮件的形式与该列表内的人员
    进行交流。更多的信息请参见：
    `Mailman page <http://www.haskell.org/mailman/listinfo/ghc-devs>`__ 。
    Subscribers can post to the list by sending their message to 
    ghc-devs@haskell.org. Further information can be found on the
    `Mailman page <http://www.haskell.org/mailman/listinfo/ghc-devs>`__.

``www.haskell.org`` 还有提供一些其它的 Haskell 和 GHC 相关的邮件列表，
请到 http://www.haskell.org/mailman/listinfo/ 页面查看完整的邮件列表。
There are several other Haskell and GHC-related mailing lists served by
``www.haskell.org``. Go to http://www.haskell.org/mailman/listinfo/
for the full list.

.. _bug-reporting:

报告 GHC 中的 bug
---------------------

.. index::
   single: bugs; reporting
   single: reporting bugs

GHC 是一个快速进化中的系统，所以 GHC 中一定会藏有 bug，如果您在GHC 中发
现了一个 bug，请根据 :ghc-wiki:`this wiki page <ReportABug>` 中的信息上
报该 bug。
Glasgow Haskell is a changing system so there are sure to be bugs in it.
If you find one, please see :ghc-wiki:`this wiki page <ReportABug>` for
information on how to report it.

.. _version-numbering:

GHC 版本号编码规则
----------------------------

.. index::
   single: version, of ghc

As of GHC version 6.8, we have adopted the following policy for
numbering GHC versions:

    Stable branches are numbered ``x.y``, where ⟨y⟩ is *even*. Releases
    on the stable branch ``x.y`` are numbered ``x.y.z``, where ⟨z⟩ (>=
    1) is the patchlevel number. Patchlevels are bug-fix releases only,
    and never change the programmer interface to any system-supplied
    code. However, if you install a new patchlevel over an old one you
    will need to recompile any code that was compiled against the old
    libraries.

    The value of ``__GLASGOW_HASKELL__`` (see :ref:`c-pre-processor`)
    for a major release ``x.y.z`` is the integer ⟨xyy⟩ (if ⟨y⟩ is a
    single digit, then a leading zero is added, so for example in
    version 6.8.2 of GHC we would have ``__GLASGOW_HASKELL__==608``).

    .. index::
       single: __GLASGOW_HASKELL__

    We may make snapshot releases of the current stable branch
    `available for
    download <http://www.haskell.org/ghc/dist/stable/dist/>`__, and the
    latest sources are available from
    :ghc-wiki:`the git repositories <Repositories>`.

    Stable snapshot releases are named ``x.y.z.YYYYMMDD``. where
    ``YYYYMMDD`` is the date of the sources from which the snapshot was
    built, and ``x.y.z+1`` is the next release to be made on that
    branch. For example, ``6.8.1.20040225`` would be a snapshot of the
    ``6.8`` branch during the development of ``6.8.2``.

    The value of ``__GLASGOW_HASKELL__`` for a snapshot release is the
    integer ⟨xyy⟩. You should never write any conditional code which
    tests for this value, however: since interfaces change on a
    day-to-day basis, and we don't have finer granularity in the values
    of ``__GLASGOW_HASKELL__``, you should only conditionally compile
    using predicates which test whether ``__GLASGOW_HASKELL__`` is equal
    to, later than, or earlier than a given major release.

    We may make snapshot releases of the HEAD `available for
    download <http://www.haskell.org/ghc/dist/current/dist/>`__, and the
    latest sources are available from
    :ghc-wiki:`the git repositories <Repositories>`.

    Unstable snapshot releases are named ``x.y.YYYYMMDD``. where
    ``YYYYMMDD`` is the date of the sources from which the snapshot was
    built. For example, ``6.7.20040225`` would be a snapshot of the HEAD
    before the creation of the ``6.8`` branch.

    The value of ``__GLASGOW_HASKELL__`` for a snapshot release is the
    integer ⟨xyy⟩. You should never write any conditional code which
    tests for this value, however: since interfaces change on a
    day-to-day basis, and we don't have finer granularity in the values
    of ``__GLASGOW_HASKELL__``, you should only conditionally compile
    using predicates which test whether ``__GLASGOW_HASKELL__`` is equal
    to, later than, or earlier than a given major release.

The version number of your copy of GHC can be found by invoking ``ghc``
with the ``--version`` flag (see :ref:`options-help`).

The compiler version can be tested within compiled code with the
``MIN_VERSION_GLASGOW_HASKELL`` CPP macro (defined only when
:ghc-flag:`-XCPP` is used). See :ref:`standard-cpp-macros` for details. 

