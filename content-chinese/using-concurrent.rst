.. _using-concurrent:

使用并行Haskell
------------------------

.. index::
   single: Concurrent Haskell; using

GHC不需要一个特殊的选项或者用一个固定的方式来编译库就能本身默认支持并行。
只需要 import：base-ref:`Control.Concurrent <Control-Concurrent.html>` 来取得并行Haskell的库的支持。
在文档里提供了更多关于并行Haskell的信息。

或者，程序可以连接到 :ghc-flag:`-threaded` 选项 （参考:ref:`options-linker`。这样做两种好处：

- 它支持:rts-flag:`-N`，这个能让线程在多处理器或者多核机器上同时（并行）运行。看 :ref:`using-smp`。

- 如果一个线程中进行了一个外部的调用（并且这个调用没有标记为``unsafe``），
  那么在这个程序中的其它Haskell线程将在这个外部调用进行的时候同时运行。
  另外，``foreign export``\ ed Haskell方程可能同时被多个OS线程调用。 参考:ref:`ffi-threads`。

以下RTS 选项 会影响并行Haskell程序的表现：

.. index::
   single: RTS options; concurrent

.. rts-flag:: -C <s>

    :default: 20 milliseconds

    设定上下文切换的时间间隔至<s>秒。
    上下文切换讲发生在下一个堆的内存块分配在计时时间逾时之后（每4千次分配发生一次堆的内存块分配）。
    用``-C0`` 或者 ``-C``， 上下文切换将尽可能多地发生 （每次堆的内存块分配都会发生）。

.. _using-smp:

用SMP平行化
---------------------

.. index::
   single: parallelism
   single: SMP

GHC支持Haskell程序在对称多处理器电脑上“平行”地运行。

有一个方法很很好地区分出*concurrency*和*parallelism*：
parallelism是关于通过同时使用多个处理器来使你的程序*更快*。而Concurrency是一种抽象的方法：
它是一个比较好的来构建一个能响应多个非同步事件的方法。

然而，这两个术语是有着密切的联系的。通过利用多CPU能让线程并行运行变为了可能，这也正是GHC的SMP平行化支持所做的事情。
但是也有可能在非并行的程序上用平行化来获得性能上的提升。这部分讲述了如何使用GHC来编译并且运行“平行”程序，在:ref:`lang-parallel`中
我们讲述了影响平行化的语言特征。

.. _parallel-compile-options:

SMP平行化的编译时间选项
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

为了用多CPU，你的程序必须连接:ghc-flag:`-threaded`选项 （参考:ref:`options-linker`）。另外，
一下编译选项影响平行化：

.. ghc-flag:: -feager-blackholing

    Blackholing is the act of marking a thunk (lazy computation) as
    being under evaluation. It is useful for three reasons: firstly it
    lets us detect certain kinds of infinite loop (the
    ``NonTermination`` exception), secondly it avoids certain kinds of
    space leak, and thirdly it avoids repeating a computation in a
    parallel program, because we can tell when a computation is already
    in progress.


    The option ``-feager-blackholing`` causes each thunk to be
    blackholed as soon as evaluation begins. The default is "lazy
    blackholing", whereby thunks are only marked as being under
    evaluation when a thread is paused for some reason. Lazy blackholing
    is typically more efficient (by 1-2% or so), because most thunks
    don't need to be blackholed. However, eager blackholing can avoid
    more repeated computation in a parallel program, and this often
    turns out to be important for parallelism.

    We recommend compiling any code that is intended to be run in
    parallel with the ``-feager-blackholing`` flag.

.. _parallel-options:

RTS options for SMP parallelism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two ways to run a program on multiple processors: call
``Control.Concurrent.setNumCapabilities`` from your program, or use the
RTS ``-N`` options.

.. rts-flag:: -N <x>
              -maxN <x>

    Use ⟨x⟩ simultaneous threads when running the program.

    The runtime manages a set of virtual processors, which we call
    *capabilities*, the number of which is determined by the ``-N``
    option. Each capability can run one Haskell thread at a time, so the
    number of capabilities is equal to the number of Haskell threads
    that can run physically in parallel. A capability is animated by one
    or more OS threads; the runtime manages a pool of OS threads for
    each capability, so that if a Haskell thread makes a foreign call
    (see :ref:`ffi-threads`) another OS thread can take over that
    capability.

    Normally ⟨x⟩ should be chosen to match the number of CPU cores on
    the machine [1]_. For example, on a dual-core machine we would
    probably use ``+RTS -N2 -RTS``.

    Omitting ⟨x⟩, i.e. ``+RTS -N -RTS``, lets the runtime choose the
    value of ⟨x⟩ itself based on how many processors are in your
    machine.

    With ``-maxN⟨x⟩``, i.e. ``+RTS -maxN3 -RTS``, the runtime will choose
    at most (x), also limited by the number of processors on the system.
    Omitting (x) is an error, if you need a default use option ``-N``.

    Be careful when using all the processors in your machine: if some of
    your processors are in use by other programs, this can actually harm
    performance rather than improve it. Asking GHC to create more capabilities
    than you have physical threads is almost always a bad idea.

    Setting ``-N`` also has the effect of enabling the parallel garbage
    collector (see :ref:`rts-options-gc`).

    The current value of the ``-N`` option is available to the Haskell
    program via ``Control.Concurrent.getNumCapabilities``, and it may be
    changed while the program is running by calling
    ``Control.Concurrent.setNumCapabilities``.

The following options affect the way the runtime schedules threads on
CPUs:

.. rts-flag:: -qa

    Use the OS's affinity facilities to try to pin OS threads to CPU
    cores.

    When this option is enabled, the OS threads for a capability *i* are
    bound to the CPU core *i* using the API provided by the OS for
    setting thread affinity. e.g. on Linux GHC uses
    ``sched_setaffinity()``.

    Depending on your workload and the other activity on the machine,
    this may or may not result in a performance improvement. We
    recommend trying it out and measuring the difference.

.. rts-flag:: -qm

    Disable automatic migration for load balancing. Normally the runtime
    will automatically try to schedule threads across the available CPUs
    to make use of idle CPUs; this option disables that behaviour. Note
    that migration only applies to threads; sparks created by ``par``
    are load-balanced separately by work-stealing.

    This option is probably only of use for concurrent programs that
    explicitly schedule threads onto CPUs with
    ``Control.Concurrent.forkOn``.

Hints for using SMP parallelism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add the :rts-flag:`-s` RTS option when running the program to see timing stats,
which will help to tell you whether your program got faster by using
more CPUs or not. If the user time is greater than the elapsed time,
then the program used more than one CPU. You should also run the program
without :rts-flag:`-N` for comparison.

The output of ``+RTS -s`` tells you how many "sparks" were created and
executed during the run of the program (see :ref:`rts-options-gc`),
which will give you an idea how well your ``par`` annotations are
working.

GHC's parallelism support has improved in 6.12.1 as a result of much
experimentation and tuning in the runtime system. We'd still be
interested to hear how well it works for you, and we're also interested
in collecting parallel programs to add to our benchmarking suite.

.. [1] Whether hyperthreading cores should be counted or not is an open
       question; please feel free to experiment and let us know what results you
       find.
