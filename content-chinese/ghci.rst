.. _ghci:


使用 GHCi
==========

.. index::
   single: GHCi
   single: 解释器
   single: 交互式
   single: Hugs
   single: 外部函数接口; GHCi 支持
   single: FFI; GHCi 支持

GHCi [1]_ 是 GHC 的交互式环境，我们可以在 GHCi 中直接输入 Haskell 表达式和程序，
代码即刻就会被解释执行并返回结果。如果你接触过 `Hugs <http://www.haskell.org/hugs/>`__ ，
就会发现 GHCi 很容易上手。此外，GHCi 还支持加载编译后的代码，同时也支持
几乎全部 [2]_ 的 GHC 的语言扩展。不仅如此，GHCi 中还包含了一个交互式调试器 (参见 :ref:`ghci-debugger`)

.. [1]
   "i" 代表交互式 (”Interactive”)

.. [2]
   当前并不包括 ``foreign export``


.. _ghci-introduction:

GHCi 简介
--------------------

让我们从运行 GHCi 开始。你可以通过 ``ghci`` 命令来打开 GHCi。

.. code-block:: none

    $ ghci
    GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
    Prelude>

由于要加载 prelude 和一些标准库，我们需要短暂等待 GHCi 完成启动并显示出提示符，
然后就像提示语上写的, 输入 :ghci-cmd:`:?` ，你就可以看到所有可用命令，
并且每个命令都会有半行的描述。后续我们会介绍其中的大部分命令，如需查看
全部命令，可以翻阅 :ref:`ghci-commands` 一节。


可以在提示符后直接输入 Haskell 表达式：

.. index::
   single: 提示符; GHCi

.. code-block:: none

    Prelude> 1+2
    3
    Prelude> let x = 42 in x / 9
    4.666666666666667
    Prelude>

GHCi 将整行代码作为表达式进行解释并计算结果。不允许多行的表达式，只要
按下回车，GHCi 就会直接执行计算。

在 Haskell 中，``let`` 表达式后面得跟着 ``in`` ，但在 GHCi 中，表达式也可以被
当作是在 ``IO`` Monad 中执行，因此 ``let`` 后面也可以不跟 ``in`` ，而得到的反馈
就是一个空行，亦如上例。（译者注：上例中并无 let 后不跟 in 的情况）


.. _loading-source-files:

加载源文件
--------------------

假设我们在 ``Main.hs`` 文件中保存了如下源代码：::

    main = print (fac 20)

    fac 0 = 1
    fac n = n * fac (n-1)

我们可以把 ``Main.hs`` 存在任何地方，但假如没有存在当前目录下 [3]_ ，
那就需要先在 GHCi 中切换到正确的目录。

.. code-block:: none

    Prelude> :cd dir

这里的 (dir) 就是你保存 ``Main.hs`` 的那个目录。

然后使用 :ghci-cmd:`:load` 来把 Haskell 源文件加载进 GHCi。

.. index::
   single: :load

.. code-block:: none

    Prelude> :load Main
    Compiling Main             ( Main.hs, interpreted )
    Ok, modules loaded: Main.
    *Main>

GHCi 加载完 ``Main`` 模块，提示符会变成 ``*Main>>`` ， 表示后续输入的表达式
都将在刚刚载入的 ``Main`` 模块的环境下运行（我们会在后面 :ref:`ghci-scope`
一章中解释这里的 ``*`` 是什么意思）。然后我们就可以使用 ``Main.hs`` 中
定义的函数来写点表达式了：

.. code-block:: none

    *Main> fac 17
    355687428096000

加载包含多模块的程序也是一样的，直接用 :ghci-cmd:`:load` 命令载入“最顶层”的
模块即可（提示：:ghci-cmd:`:load` 可以被简写为 ``:l`` ）。通常最顶层的模块
就是 ``Main`` ，但也可以是其它的。GHCi 会通过最顶层的模块来判断，哪些模块
被引用了，直接的还是间接的，并把它们按照依赖顺序加载进来。

.. [3]
   如果是从命令行启动 GHCi，GHCi 的当前目录就是 shell 所在的目录。
   如果是从 Windows 的开始菜单启动 GHCi，那当前目录就可能是类似这样的
   ``C:\Documents and Settings\user name``.


.. _ghci-modules-filenames:

模块 vs. 文件名
~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: 模块; 与文件名
   single: 文件名; 模块的

问：GHC 是如何找到包含模块 (M) 的文件的？
答：GHC 会去找名为 ``M.hs`` 或 ``M.lhs`` 的文件。所以对大部分模块来说，
你的模块名需要和文件名保持一致。否则，GHC 就没法找到它。

此通用规则有一个例外：当你通过 :ghci-cmd:`:load` 加载一个程序，或直接在
启动 ``ghci`` 时就指定一个待加载程序时，你也可以不用模块名，而是直接使用
文件名。只要这个文件名存在，那无论里面的模块名是什么，都会被加载。这一点
对于同一文件夹里包含多个 ``Main`` 模块的情况，就会很方便，毕竟我们不能把
这些文件都命名为 ``Main.hs`` 。

可以在 GHCi 命令行中通过 :ghc-flag:`-i` 来指定查找源文件的搜索路径，比如：

.. code-block:: none

    ghci -idir1:...:dirn

也可以在 GHCi 中通过 :ghci-cmd:`:set` 命令来设置 （参见 :ref:`ghci-cmd-line-options` ）[4]_

GHCi 通过依赖关系来确定要加载的模块，因此这使得我们需要满足，一个模块一个文件。
唯一的例外是来那些自于包 (package) 的模块，其中包括 ``Prelude`` 和一些标准库，
如比 ``IO`` 和 ``Complex`` 。如果 GHCi 找不到你想要加载的模块的源文件，即使
存在该模块的目标文件和接口文件，你也会得到一个错误信息，

.. [4]
   注意，在GHCi 中，如果开启了 :ghc-flag:`--make` 模式，那 :ghc-flag:`-i` 就是
   用来指定*源*文件的搜索路径的，而在标准的分批编译模式下， :ghc-flag:`-i` 则是
   用来指定接口文件的搜索路径的，参见 :ref:`search-path` 。


文件修改与重新编译
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: :reload

如果你修改了源文件并希望 GHCi 能够重新编译程序，此时就可以使用 :ghci-cmd:`:reload` 命令。
程序会按需要进行重新编译，而实际上，GHCi 会尽量避免在外部依赖没有变化的情况下重新编译模块。
在分批编译的模式下，也有同样的机制来避免没有必要的重新编译（参见 :ref:`recomp` ）。

.. _ghci-compiled:

加载已编译代码
---------------------

.. index::
   single: 已编译代码; 在 GHCi 中

当你把一个 Haskell 源文件载入 GHCi，它通常会被转成字节码，然后通过解释器运行。
不过，解释后的代码也可以和编译后的代码一起在 GHCi 中运行；实际一般情况下，
GHCi 在启动时就会去加载已经编译完了的 ``base`` 包的代码，其中就包含了 ``Prelude`` 。

那我们有什么理由要去使用编译后的代码呢？原因很简单，编译后的代码，其运行速度大概比
解释后的代码要快上10倍。不过缺点是编译所消耗的时间大概是解释的2倍（如果开启优化，时间可能更长）。
所以值得考虑的做法是，编译那些很少会改动的代码，然后用解释器去运行开发中的代码。

当我们通过 :ghci-cmd:`:load` 去加载代码模块时， GHCi 通常会先看一下有没有对应的编译后的
目标文件，如果有就尽量不去解释源文件。举例来说，假设我们有一段程序，包含4个模块，分别是 ``A``、
``B``、``C`` 和 ``D``。模块 ``B`` 和模块 ``C`` 同时都只引用了模块 ``D`` ，而模块 ``A``
引用了模块 ``B`` 和模块 ``C`` 。


.. code-block:: none

          A
         / \
        B   C
         \ /
          D

我们可以先编译 ``D``，然后再加载整个程序，如下：

.. code-block:: none

    Prelude> :! ghc -c -dynamic D.hs
    Prelude> :load A
    Compiling B                ( B.hs, interpreted )
    Compiling C                ( C.hs, interpreted )
    Compiling A                ( A.hs, interpreted )
    Ok, modules loaded: A, B, C, D (D.o).
    *Main>

从编译器给出的信息我们可以看出，这里并没有提到 ``D``。原因是自从上次编译之后，它的源代码及
它所依赖的代码都没有变化，所以就没有必要再去重新编译 ``D`` 了。

注意这里的 GHC :ghc-flag:`-dynamic` 开关：GHCi 使用动态链接的目标代码 （如果你所在的系统支持的
话），所以如果要让 GHCi 使用编译后的代码，就需要开启动态链接的开关来进行编译。


你可以随时使用 :ghci-cmd:`:show modules` 命令来查看当前已加载进 GHCi 的模块：

.. code-block:: none

    *Main> :show modules
    D                ( D.hs, D.o )
    C                ( C.hs, interpreted )
    B                ( B.hs, interpreted )
    A                ( A.hs, interpreted )
    *Main>

如果我们现在修改模块 ``D`` 的源代码（也可以用 Unix 命令在源文件上 ``touch`` 一下来方便地
假装修改），编译器就没办法再去使用那个已过期的目标文件了。

.. code-block:: none

    *Main> :! touch D.hs
    *Main> :reload
    Compiling D                ( D.hs, interpreted )
    Ok, modules loaded: A, B, C, D.
    *Main>

注意，虽然模块 ``D`` 被重新编译了，但 ``touch`` 之后源代码并没有改变，生成的接口文件还
和之前一样，所以模块 ``A``、模块 ``B``和模块 ``C`` 就不用再重新编译了，这个判断由重新编译的
检查器来下。

然后让我们试着挑一个其它模块去编译：

.. code-block:: none

    *Main> :! ghc -c C.hs
    *Main> :load A
    Compiling D                ( D.hs, interpreted )
    Compiling B                ( B.hs, interpreted )
    Compiling C                ( C.hs, interpreted )
    Compiling A                ( A.hs, interpreted )
    Ok, modules loaded: A, B, C, D.

竟然并没有使用模块 ``C`` 的编译后代码！这是怎么回事？原来在 GHCi 中，一个编译后的模块只能
依赖于其他编译后的模块，而在我们上面的场景中，模块 ``C`` 依赖于模块 ``D``，但 ``D`` 并没有
对应的目标文件，所以 GHCi 就顺势不去使用 ``C`` 的目标文件了。好了，接下来让我们再来编译 ``D``。

.. code-block:: none

    *Main> :! ghc -c D.hs
    *Main> :reload
    Ok, modules loaded: A, B, C, D.

什么都没发生！这又是另一课：:ghci-cmd:`:reload` 并不会使用新编译出来的模块，只有
:ghci-cmd:`:load` 会去使用它们：

.. code-block:: none

    *Main> :load A
    Compiling B                ( B.hs, interpreted )
    Compiling A                ( A.hs, interpreted )
    Ok, modules loaded: A, B, C (C.o), D (D.o).

目标文件的自动加载，有时候也会造成混乱，这是因为只有在解释一个模块的代码时，我们才能在 GHCi 提
示符下输入表达式，使用模块中未输出的那部分定义。所以，有时候你可能会希望强制 GHCi 使用解释器去
加载一个模块。使用 :ghci-cmd:`:load` 加载模块时，在模块名或文件名前加上 ``*`` 前缀就可以
做到这一点，例如：

.. code-block:: none

    Prelude> :load *A
    Compiling A                ( A.hs, interpreted )
    *A>

用了 ``*`` 之后，GHCi 就会忽略那些预编译的目标代码，并去解释执行这个模块。如果你已经加载了
一堆模块的目标代码，然后想要去解释执行其中一个模块，你并不用重新加载所有模块，而是可以用
``:add *M`` 来指定说，我就想解释执行 ``M`` 这一个模块（注意，这一步也可能导致其他模块被解释
执行，因为编译后的模块不能依赖于解释后的模块）。

如果想要把所有东西都编译成目标代码，而不去使用解释器，那就要使用 :ghc-flag:`-fobject-code`
开关（参见 :ref:`ghci-obj`）。

.. hint::
    只有在 GHCi 确定编译后的版本没有过期时，才会去使用编译后的目标文件。在一个大型程序的开发
    中，有一个很好的技巧，即时不时地运行 ``ghc --make`` 去编译整个项目（比如在你去吃午饭
    前），然后接着用解释器去跑你的代码。每次你修改代码，对应的模块就会被解释执行，而项目中
    的其他模块还是用的编译后代码。

.. _interactive-evaluation:

提示符下的交互式求值计算
------------------------------------

每次我们在提示符下输入一个表达式，GHCi 就会立即对其进行求值计算，并把结果打印出来：

.. code-block:: none

    Prelude> reverse "hello"
    "olleh"
    Prelude> 5+5
    10

.. _actions-at-prompt:

提示符下的 I/O 操作
~~~~~~~~~~~~~~~~~~~~~~~~~

GHCi 能做的还不仅仅时简单的表达式计算。如果你输入一个 ``IO a`` 类型的表达式，其中 ``a`` 是
一个确定的类型，那 GHCi 就会把它当作 IO 操作来*执行*。

.. code-block:: none

    Prelude> "hello"
    "hello"
    Prelude> putStrLn "hello"
    hello

不仅如此，即使表达式是个泛型，只要它可以被*实例化*为 ``IO a`` 类型，就也能被正确计算，比如：

.. code-block:: none

    Prelude> return True
    True

此外，当且仅当满足以下条件时，GHCi 会将 I/O 操作的结果打印出来：

-  结果的类型是 ``Show`` 的实例。

-  结果的类型不是 ``()``。

举例来说，我们可以回忆一下 ``putStrLn :: String -> IO ()`` ：

.. code-block:: none

    Prelude> putStrLn "hello"
    hello
    Prelude> do { putStrLn "hello"; return "yes" }
    hello
    "yes"

.. _ghci-stmts:

Using ``do`` notation at the prompt
在提示符下使用 ``do`` 语法
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: do 语法; 在 GHCi 中
   single: 语句; 在 GHCi 中

在 GHCi 提示符下，我们不仅可以输入表达式，还可以输入语句。也就是说，你可以将值和函数绑定到
名字上，以便后续在表达式或其他语句中使用。

在 GHCi 提示符下，语句的语法和 do 表达式中语句的语法完全一样。不过，这里不存在 Monad 重载，
也就是说，输入的语句的所在环境只能是 ``IO`` Monad。

.. code-block:: none

    Prelude> x <- return 42
    Prelude> print x
    42
    Prelude>

语句 ``x <- return 42`` 表示"在 ``IO`` monad 中执行 ``return 42``，并把结果绑定到
``x`` 上"。然后我们就可以在后续的语句中使用 ``x`` 了，比如在上面的代码中我们可以把它打印出来。

.. ghc-flag:: -fprint-bind-result

    如果开启 :ghc-flag:`-fprint-bind-result`，则当且仅当满足以下条件，GHCi 才会打印出
    一个语句的结果：

    - 该语句不是一个绑定，除非它是只绑定一个变量的 monad 式绑定 (``p <- e``)。

    - 绑定变量的类型既不是多态的，也不是 ``()``，完了还得是 ``Show`` 的实例。


当然了，你也可以使用 ``let`` 来绑定常规的非 IO 表达式：

.. code-block:: none

    Prelude> let x = 42
    Prelude> x
    42
    Prelude>

以上两类绑定之间另一个重要的区别在于，monad 式绑定 (``p <- e``) 是严格的 (即 ``e`` 会直接被
求值)，而对于 ``let`` 形式的绑定，表达式并不会被立即求值计算：

.. code-block:: none

    Prelude> let x = error "help!"
    Prelude> print x
    *** Exception: help!
    Prelude>

注意，``let`` 形式的绑定并不会自动打印所绑定的值，这一点与 ``monad`` 式的绑定不同。

你也可以在提示符下定义函数：

.. code-block:: none

    Prelude> add a b = a + b
    Prelude> add 1 2
    3
    Prelude>

不过，一旦要开始写多子句的函数，或者几个相互递归调用的函数时，事情就会开始变得有些麻烦，你就必须
用分号来分隔语句，这是因为我们必须在一行内提供完整的函数定义。

.. code-block:: none

    Prelude> f op n [] = n ; f op n (h:t) = h `op` f op n t
    Prelude> f (+) 0 [1..3]
    6
    Prelude>

当然你也可以不用上面这种蛋疼的写法，我们也可以把一堆 GHCi 命令包在 ``:{`` 与 ``:}`` 中，
这样，这些命令就可以写成多行的形式了 (``:{`` 与 ``:}`` 必须单独各占一行)。

.. code-block:: none

    Prelude> :{
    Prelude| g op n [] = n
    Prelude| g op n (h:t) = h `op` g op n t
    Prelude| :}
    Prelude> g (*) 1 [1..3]
    6

任何 GHCi 命令都可以被用在多行形式中，值得注意的是，在多行命令中要遵守排版规则。提供多行命令
的目的，并不是要代替模块加载，而是要让 .ghci 文件 (参见：:ref:`ghci-dot-files`) 中的定义
更加可读和可维护。

GHCi 会捕获并打印出在语句求值和执行过程中产生的任何异常 (更多关于异常的内容，参见
:base-ref:`documentation <Control-Exception.html>` 库中的 ``Control.Exception``
模块)。

新的绑定会覆盖之前同名的绑定，其中也包括在当前加载的模块中定义的内容。

.. warning::
    提示符下定义的临时绑定，在下次执行 :ghci-cmd:`:load` 或 :ghci-cmd:`:reload` 命令
    时就会失效。不过，当我们使用 :ghci-cmd:`:module`: 切换模块环境时，临时绑定会迁移到新的
    环境中继续生效。

.. hint::
    可以使用 :ghci-cmd:`:show bindings` 命令来查看当前环境下的所有绑定。

    .. code-block:: none

        Prelude> :show bindings
        x :: Int
        Prelude>

.. hint::
    如果你开启 ``+t`` 选项，GHCi 会在产生绑定的语句后打印出每一个变量的类型。例如：

    .. code-block:: none

        Prelude> :set +t
        Prelude> let (x:xs) = [1..]
        x :: Integer
        xs :: [Integer]

    .. index::
        single: +t option; in GHCi


.. _ghci-multiline:

多行输入
~~~~~~~~~~~~~~~

对于多行输入，除了上面提到的 ``:{ ... :}`` 语法外，GHCi 还提供了一种多行模式，通过
``:set +m`` 即可以开启这一模式。启用 ``:set +m`` 后，GHCi 会自动判断当前语句是否还未
结束，并允许在下一行中继续输入。整个语句的结束，是通过一个空行来标识的。例如：

.. code-block:: none

    Prelude> :set +m
    Prelude> let x = 42
    Prelude|

我们还可以在整个 ``let`` 语句里，加入更多的绑定，所以 GHCi 在这里把提示符换成了竖线，用来
表示下一行输入将延续上面的语句。注意，在多行模式中，排版规则依然生效，所以我们需要给后续输入的
绑定增加缩进。

.. code-block:: none

    Prelude> :set +m
    Prelude> let x = 42
    Prelude|     y = 3
    Prelude|
    Prelude>

如果不想使用缩进排版，也可以使用大括号和分号：

.. code-block:: none

    Prelude> do {
    Prelude| putStrLn "hello"
    Prelude| ;putStrLn "world"
    Prelude| }
    hello
    world
    Prelude>

注意，输入完闭合的大括号，GHCi 就知道了当前语句已经结束，此时就不再需要空行结尾了。

在输入 monad 式 ``do`` 语句时，多行模式还是很有用的：

.. code-block:: none

    Control.Monad.State> flip evalStateT 0 $ do
    Control.Monad.State| i <- get
    Control.Monad.State| lift $ do
    Control.Monad.State|   putStrLn "Hello World!"
    Control.Monad.State|   print i
    Control.Monad.State|
    "Hello World!"
    0
    Control.Monad.State>

在多行模式的输入中，用户也可以打断多行模式，回到顶层的提示符。

.. code-block:: none

    Prelude> do
    Prelude| putStrLn "Hello, World!"
    Prelude| ^C
    Prelude>

.. _ghci-decls:

类型，类型类以及其他声明
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

在 GHCi 提示符下，你可以输入任何顶层的 Haskell 声明，其中包括 ``data``、``type``、
``newtype``、``class``、``instance``、``deriving`` 以及 ``foreign`` 声明。例如：


.. code-block:: none

    Prelude> data T = A | B | C deriving (Eq, Ord, Show, Enum)
    Prelude> [A ..]
    [A,B,C]
    Prelude> :i T
    data T = A | B | C      -- Defined at <interactive>:2:6
    instance Enum T -- Defined at <interactive>:2:45
    instance Eq T -- Defined at <interactive>:2:30
    instance Ord T -- Defined at <interactive>:2:34
    instance Show T -- Defined at <interactive>:2:39

对于普通的变量绑定，后面的定义会覆盖之前的定义，这让我们可以不断重复输入它的新定义，来修复
任何问题或者是做进一步的扩展。但对于一个类型，当我们输入一个新定义时，问题就来了。我们也
可以试图用新的类型定义来覆盖老的定义，不过问题是，如果之前存在其他函数或类型的定义使用到了
这个老的定义，在新定义生效后，它们仍将使用老的定义。虽然新老定义的名字相同，但 GHCi 却
认为它们是不同的。例如：

.. code-block:: none

    Prelude> data T = A | B
    Prelude> let f A = True; f B = False
    Prelude> data T = A | B | C
    Prelude> f A

    <interactive>:2:3:
        Couldn't match expected type `main::Interactive.T'
                    with actual type `T'
        In the first argument of `f', namely `A'
        In the expression: f A
        In an equation for `it': it = f A
    Prelude>

在这里，GHCi 用 ``main::Interactive.T`` 来表示那个被覆盖了的上一个版本的 ``T``，这样就
可以和新的 ``T`` 区分开。而新的 ``T`` 就可以直接表示为 ``T``。

类型类 (class) 和类型族 (type-family) 的实例声明会被简单地加入可用实例列表中。不过有一个
例外，一个类型类或类型族的新实例，会把之前定义过的和它类型相同的实例给替换掉 (参见：
:ref:`type-families`)。


.. _ghci-scope:

提示符当前作用域下到底有些什么？
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

当你在提示符下输入一个表达式，当前作用域下究竟存在哪些类型和标识符？GHCi 提供了非常灵活的方式
来准确控制一个表达式的上下文环境该如何构建：

-  :ghci-cmd:`:load`、 :ghci-cmd:`:add` 以及 :ghci-cmd:`:reload` 命令 (参见
    :ref:`ghci-load-scope`)。

-  ``import`` 声明 (参见 :ref:`ghci-import-decl`)。

-  :ghci-cmd:`:module` 命令 (参见 :ref:`ghci-module-cmd`)。

可以使用 :ghci-cmd:`:show imports` 命令来查看，顶层作用域中都有哪些模块。

.. hint::
    GHCi 允许使用 tab 来补全作用域中的名字；例如，如果你在 GHCi 中输入 ``J<tab>``，
    GHCi 会将其展开为 ``Just``。

.. _ghci-load-scope:

``:load`` 对作用域的影响
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:ghci-cmd:`:load`、 :ghci-cmd:`:add` 以及 :ghci-cmd:`:reload` 命令
(参见 :ref:`loading-source-files` 以及 :ref:`ghci-compiled`) 都会对顶层作用域
产生影响。让我们从一些简单地情况入手；当你启动 GHCi 时，提示符是这样的：

.. code-block:: none

    Prelude>

这表示 ``Prelude`` 模块中的所有内容正存在于当前作用域；而这些可见的标识符，恰好等同于没有
任何 ``import`` 声明的 Haskell 源文件中的情况。

如果此时我们将一个文件载入 GHCi，提示符就会变为：

.. code-block:: none

    Prelude> :load Main.hs
    Compiling Main             ( Main.hs, interpreted )
    *Main>

提示符变成了 ``*Main``，这表示后续输入的表达式将在 ``Main`` 模块的顶层上下文环境中进行
求值计算。``Main`` 模块的顶层作用域下的所有内容都会被载入当前提示符作用域 (只要 ``Main``
没有隐藏 ``Prelude``，``Prelude`` 的内容也会存在)。

提示符中类似 ``*module`` 的字样，表示此模块的顶层作用域，即为当前提示符作用域中的全部内容。
如果此处没有那个 ``*``，则表示只有该模块的输出内容才是可见的。

.. note::
    由于技术原因，GHCi 在解释的方式去载入模块时，只支持带 ``*`` 的形式。而编译后的模块和包
    则只支持载入输出项。如果希望 GHCi 去解释一个模块，可以在加载模块时加上 ``*``，
    例如 ``:load *M``。

通常在使用 :ghci-cmd:`:load` 命令时，会将最后一个加载的模块自动载入当前作用域，并尽量使用
``*`` 形式。例如，如果输入 ``:load foo.hs bar.hs`` 且 ``bar.hs`` 包含 ``Bar`` 模块，
则如果 ``Bar`` 是被解释后载入的，当前作用域将设置为 ``*Bar``，如果载入的是
编译后的 ``Bar``，则会设置为 ``Prelude Bar`` （如果没有任何 ``*`` 形式的模块，且没有显示
地载入 ``Prelude``, GHCi 就会自动加上 ``Prelude`` )。使用 :ghci-cmd:`:show imports`:
可以查看这些被自动载入的模块。

.. code-block:: none

    Prelude> :load hello.hs
    [1 of 1] Compiling Main             ( hello.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> :show imports
    :module +*Main -- added automatically
    *Main>

下次你再使用 :ghci-cmd:`:load`、 :ghci-cmd:`:add` 以及 :ghci-cmd:`:reload` 命令，
这些自动载入的模块又会被替换掉。也可以像对常规载入一样，使用 :ghci-cmd:`:module` 命令来
移除已载入的模块。

.. _ghci-import-decl:

通过 ``import`` 来控制作用域中的内容
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

我们并非只能载入一个模块，GHCi 可以合并多个模块的作用域，无论它们各自是否以 ``*`` 形式载入。
GHCi 会合并这些模块的作用域，作为提示符下真正的作用域。

可以使用常规的 Haskell ``import`` 语法来把模块加载进当前作用域。

.. code-block:: none

    Prelude> import System.IO
    Prelude System.IO> hPutStrLn stdout "hello\n"
    hello
    Prelude System.IO>

这里支持 Haskell 的 import 的全部语法，包括 ``hiding`` 和 ``as`` 子句。提示符会显示当前
被加载的模块，但不会显示 ``hiding`` 和 ``as`` 的细节。想要看完整细节，可以使用
:ghci-cmd:`:show imports` 命令。

.. code-block:: none

    Prelude> import System.IO
    Prelude System.IO> import Data.Map as Map
    Prelude System.IO Map> :show imports
    import Prelude -- implicit
    import System.IO
    import Data.Map as Map
    Prelude System.IO Map>

注意，这里 ``Prelude`` 的载入被标为隐式 (implicit)，你也可以显示地载入 ``Prelude`` 来
覆盖它。这一点和在普通 Haskell 模块中一样。

当作用域中有多个被载入的模块，尤其是当这些模块又都是使用 ``*`` 形式载入的，这时就很有可能产生
命名冲突。在 Haskell 中，只用在使用到这些有歧义的标识符时，才会报命名冲突。在这一点上 GHCi
也是这么做的。

.. _ghci-module-cmd:

通过 ``:module`` 命令来控制作用域中的内容
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

另外一个操控作用域的方法是使用 :ghci-cmd:`:module` 命令，其语法如下：

.. code-block:: none

    :module +|- *mod1 ... *modn

在 ``module`` 命令中，可以使用 ``+`` 来把模块增加进当前作用域，使用 ``-`` 来把模块从
当前作用域中移除。如果不加 ``+`` 和 ``-``，则当前作用域将替换为命令中指定的那些模块。
注意，如果你使用此方法替换模块且其中没有包含 ``Prelude``，GHCi 会自动隐式载入 ``Prelude``。

:ghci-cmd:`:module` 有两件事情是 ``import`` 做不到的：

-  :ghci-cmd:`:module` 支持 ``*`` 修饰符，可以载入模块顶层作用域中的内容，而非仅仅载入
   其输出项。

-  使用 ``:module -M`` 可以移除 ``import`` 的模块。由于 ``import`` 命令和在 Haskell
   模块中一样是累加型的导入，没有替换或删除的功能，所以这就成了唯一能移除 ``import`` 导入的
   模块的方法。

.. _ghci-import-qualified:

Qualified 名字
^^^^^^^^^^^^^^^

为了减少麻烦，GHCi 在载入任何模块时，都仿佛是在使用了一个隐式的 ``import qualified``。
你也可以通过 ``-fno-implicit-import-qualified`` 开关来关闭这个功能。

.. index::
   single: -fno-implicit-import-qualified

``:module`` 与 ``:load``
^^^^^^^^^^^^^^^^^^^^^^^^^

看上去好像 :ghci-cmd:`:module`/``import`` 和
:ghci-cmd:`:load`/:ghci-cmd:`:add`/:ghci-cmd:`:reload` 做的事情很相似：两种方式
都可以让你把模块载入作用域。但是其中有一个非常重要的区别。在 GHCi 中有两个集合的模块：

-  一个集合的模块是当前*被载入的*。:ghci-cmd:`:load`、:ghci-cmd:`:add` 和
   :ghci-cmd:`:reload` 修改的就是这一集合，并可以通过 :ghci-cmd:`:show modules`
   来查看。

-  另一个集合的模块是当前提示符*作用域中的*。``import`` 和 :ghci-cmd:`:module` 修改的就是
   这一集合，而在执行 :ghci-cmd:`:load`、:ghci-cmd:`:add` 和 :ghci-cmd:`:reload`
   之后也会自动修改这一集合。可以通过 :ghci-cmd:`:show imports` 来查看作用域中的模块。

只有在两种情况下才能将一个模块加入作用域 (通过 :ghci-cmd:`:module` 或 ``import``)，
满足 (a) 该模块已被加载，或者 (b) 该模块来自于 GHCi 认识的包。如果尝试用
:ghci-cmd:`:module` 或 ``import`` 去把一个尚未加载的模块加入作用域，则会报出这样的信息：
``模块 M 尚未被加载`` (``module M is not loaded``)。

``:main`` 和 ``:run`` 命令
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

当一个程序被编译并执行时，它可以使用 ``getArgs`` 函数来获取命令行参数。不过，当我们在 GHCi 中
测试 ``main`` 函数时，并不能这么简单地把参数传给它，因为当前并不是命令行环境。

要做到这点，我们需要使用 :ghci-cmd:`:main` 命令。该命令会执行当前作用域下的 ``main`` 函数，
并把命令本身收到的参数作为命令行参数传递给 ``main``，举例如下：

.. code-block:: none

    Prelude> main = System.Environment.getArgs >>= print
    Prelude> :main foo bar
    ["foo","bar"]

我们也可以给包含空格的参数加上引号，它们会被当作正常的 Haskell 字符串。另外，我们还可以
使用数组来书写：

.. code-block:: none

    Prelude> :main foo "bar baz"
    ["foo","bar baz"]
    Prelude> :main ["foo", "bar baz"]
    ["foo","bar baz"]

最后，我们还可以把其他函数当做 ``main`` 来执行：可以使用 ``-main-is`` 把任意一个函数
设置为 ``main`` 函数并用 :ghci-cmd:`:main` 来执行，也可以使用 :ghci-cmd:`:run` 直接
调用。

.. code-block:: none

    Prelude> foo = putStrLn "foo" >> System.Environment.getArgs >>= print
    Prelude> bar = putStrLn "bar" >> System.Environment.getArgs >>= print
    Prelude> :set -main-is foo
    Prelude> :main foo "bar baz"
    foo
    ["foo","bar baz"]
    Prelude> :run bar ["foo", "bar baz"]
    bar
    ["foo","bar baz"]

``it`` 变量
~~~~~~~~~~~~~~~~~~~

.. index::
   single: it 变量

当我们在提示符输入一个表达式 (或者准确地说，是一个非绑定语句) 时，GHCi 会隐式地把运算结果
绑定到变量 ``it`` 上。例如：

.. code-block:: none

    Prelude> 1+2
    3
    Prelude> it * 2
    6

此时实际发生的是，GHCi 会检查这个表达式的类型，如果不是 ``IO`` 类型，就会将你输入的
表达式 ``e`` 转化为下面的内容：

.. code-block:: none

    let it = e;
    print it

这两句自然是作为一个 IO 操作来执行的。

因此，原来你输入的表达式的类型，必须是 ``Show`` 的实例，否则 GHCi 会报错：

.. code-block:: none

    Prelude> id

    <interactive>:1:0:
        No instance for (Show (a -> a))
          arising from use of `print' at <interactive>:1:0-1
        Possible fix: add an instance declaration for (Show (a -> a))
        In the expression: print it
        In a 'do' expression: print it

从错误信息中，我们也可以看出这个内部变换的一些端倪。

如果这个表达式的类型是 ``IO a``，那 ``it`` 就会被绑定到这个 IO 计算的结果上，其类型
是 ``a``。例如：

.. code-block:: none

    Prelude> Time.getClockTime
    Wed Mar 14 12:23:13 GMT 2001
    Prelude> print it
    Wed Mar 14 12:23:13 GMT 2001

IO 类型的表达式 ``e``，经过的内部变换就是这样的了：

.. code-block:: none

    it <- e

注意，每次求值计算一个新的表达式，``it`` 都会被覆盖，之前的值就会丢失。

.. _extended-default-rules:

GHCi 中类型的默认具体化
~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: 类型默认策略; GHCi 中
   single: Show 类型类

.. ghc-flag:: -XExtendedDefaultRules

    允许数字类型类以外的情况下，触发类型的默认具体化

考虑如下 GHCi 会话：

.. code-block:: none

      ghci> reverse []

GHCi 会做些什么？严格来说，这段程序是有歧义的。 ``show (reverse [])`` (GHCi 会计算这个)
的类型是 ``Show a => String``，而最终显示的结果取决于 ``a`` 的类型。例如：

.. code-block:: none

      ghci> reverse ([] :: String)
      ""
      ghci> reverse ([] :: [Int])
      []

不过，如果总是需要用户来指定类型还是有点麻烦，所以 GHCi 就扩展了 Haskell 的默认类型策略的
规则 (Haskell 2010 report 的 4.3.4 小节)，具体如下。标准规则会参考每个类型变量 ``a`` 上
的限制条件 ``(C1 a, C2 a, ..., Cn a)``，只有满足以下条件，才会给该类型变量一个默认类型：

1. 类型变量 ``a`` 不再其它限制条件中出现。

2. 所有 ``Ci`` 都是标准类型类 (译者注：标准类型类，参见 Haskell 2010 report，6.3 小节)。

3. `Ci` 中至少有一个类型类是数字类型的 (numeric)。

而在 GHCi 提示符下，也包括 :ghc-flag:`-XExtendedDefaultRules` 标记的 GHC，额外增加
了以下规则：

-  放宽上面的第二条规则：所有类型类 ``Ci`` 只需要是单参数类型类即可。

-  放宽上面的第三条规则：``Ci`` 中至少有一个是*交互式类型类* (下一小节有定义)

-  在做类型的默认具体化时，标准类型中增加了，单元类型 (unit type) ``()`` 和
   列表类型 ``[]``。

下面这个程序很好地反映了最后一点的作用：

    main :: IO ()
    main = print def

    instance Num ()

    def :: (Num a, Enum a) => a
    def = toEnum 0

此代码会打印出 ``()`` 而不是 ``0``，因为 ``def`` 的类型被默认为 ``()``，
而不是 ``Integer``。

这样修改的目的，是为了让 ``IO a`` 默认类型变成 ``IO ()``，这样 GHCi 就不需要在
执行这个 IO 的时候把结果打印出来了。这一点对 ``printf`` 来说很重要，因为
它有个实例，返回的类型就是 ``IO a``。不过它只能返回 ``undefined`` (之所以会有这个
类型的实例，是为了让 `printf` 不依赖任何类型系统的扩展)，所以如果把类型默认为
``Integer``，那 GHCi 在运行 printf 时就会报错。

关于处在 monad 中的计算型表达式是如何尽可能默认为 ``IO`` 类型的，
也可参考 :ref:`actions-at-prompt`。

交互式类型类
^^^^^^^^^^^^^^^^^^^

.. index::
   single: 交互式类型类

交互式类型类 (开启 :ghc-flag:`-XExtendedDefaultRules` 时才会涉及此概念) 包括：
所有数字类型类、``Show``、``Eq``、``Ord``、``Foldable`` 以及 ``Traversable``。

只有一个类型变量受到以上类型类的约束，类型的默认具体化就会被触发，大致策略如上述。


``default`` 声明相关的扩展规则
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index::
   single: default 声明

在开启 :ghc-flag:`-XExtendedDefaultRules 的情况下，除了类型的默认具体化规则被放宽，
``default`` 声明的规则也被放宽了。根据 Haskell 2010 Report 中 4.3.4 小节所描述的，
一个 ``default`` 声明形如 ``default (t1, ..., tn)``，对于其中每一个 ``ti``，都
必须满足 ``Num ti`` 成立。而这条规则也会被放宽为，对于每一个 ``ti``，存在一个交互式
类型类 ``C``，是的 ``C ti`` 成立即可。这意味着 ``default`` 的参数中就可以使用
类型构造器了。例如，如果你希望 ``Foldable`` 约束默认都使用 ``Maybe`` 类型，而 ``Num``
约束仍然默认为 ``Integer`` 或 ``Double``，那就可以像下面这么写：

    default (Maybe, Integer, Double)

.. _ghci-interactive-print:

Using a custom interactive printing function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: Custom printing function; in GHCi

自从 GHC 7.6.1，GHCi 就开始使用 ``System.IO.print`` 来打印提示符下输入的表达式的结果。
它的类型签名是 ``Show a => a -> IO ()``，其工作机制就是用 ``show`` 把结果
转为 ``String``。

在某些场景下，这并不是最理想的处理方式，比如对于很长的输出，或是包含非 ascii 码的字符。

使用 :ghc-flag:`-interactive-print` 标记，可以把任何类型为 ``C a => a -> IO ()`` 的
函数设置为求值结果的打印函数。该函数可以放在任何已加载的模块或是已注册的包中。不过只有当该函数
是处在一个已注册的包中时，:ghci-cmd:`:cd`、:ghci-cmd:`:add`、:ghci-cmd:`:load`、
:ghci-cmd:`:reload` 以及 :ghci-cmd:`:set` 命令才不会影响其使用。

.. ghc-flag:: -interactive-print <expr>

    Set the function used by GHCi to print evaluation results. Expression
    must be of type ``C a => a -> IO ()``.

作为示例，假设我们有如下一个特殊的打印模块：

    module SpecPrinter where
    import System.IO

    sprint a = putStrLn $ show a ++ "!"

``sprint`` 函数会在每一个打印结果后加上一个感叹号。使用如下命令启动 GHCi：

.. code-block:: none

    ghci -interactive-print=SpecPrinter.sprint SpecPrinter

这样就会启动一个使用 ``sprint`` 作为打印函数的会话：

.. code-block:: none

    *SpecPrinter> [1,2,3]
    [1,2,3]!
    *SpecPrinter> 42
    42!

当然，也可以使用一些格式化打印的函数，例如，把树形嵌套结构的数据类型，格式化为更易读的输出。

GHC 在 ``-e`` 模式下也可使用 :ghc-flag:`-interactive-print` 标记：

.. code-block:: none

    % ghc -e "[1,2,3]" -interactive-print=SpecPrinter.sprint SpecPrinter
    [1,2,3]!

.. _ghci-stack-traces:

GHCi 中的调用栈跟踪
~~~~~~~~~~~~~~~~~~~~

.. index::
  simple: 调用栈跟踪; GHCi 中

[ 本功能是在 GHC 8.0.1 中引入的实验功能，可以通过 ``-fexternal-interpreter`` 标记开启，
  目前尚不支持 Windows。]

GHCi 可以通过性能分析系统 (profiling system) 来收集在运行解释型代码时的调用栈跟踪信息。
想要开启这个功能，请按照如下命令启动 GHCi:

.. code-block:: none

    ghci -fexternal-interpreter -prof

这样就会在一个单独的进程中运行解释型代码 (参见 :ref:`external-interpreter`)，并开启性能
分析模式来收集调用栈信息。注意，由于我们是在性能分析模式下运行解释型代码，因此你使用的所有的包
都必须是为性能分析而编译的版本。GHCi 的 ``-prof`` 标记只有在和
``-fexternal-interpreter`` 一起使用时才管用。

有三种方式可以获取当前的调用栈。

- ``error`` 和 ``undefined`` 会自动调取当前的调用栈，并附加到错误信息中。通常这里还会
  带上 ``HasCallStack`` 的调用栈信息 (参见 :ref:`hascallstack`)，因此两个调用栈都会
  被输出。

- ``Debug.Trace.traceStack`` 是 ``Debug.Trace.trace`` 的一种形式，它也会打印出
  当前调用栈。

- ``GHC.Stack`` 模块中的函数，也可以被用来获取并输出当前调用栈。

对于解释型的模块，你不必显示地使用 ``-fprof-auto``，GHCi 会自动为代码加上注释，并且
粒度足够细，可以区分出每一个调用点 (call site)。不过，你可能会看不到编译型代码的调用栈
信息，除非它们是开启 ``-fprof-auto`` 进行编译的，或者显示地使用了 ``SCC` 标记 (参见
:ref:`scc-pragma`)。

.. _ghci-debugger:

The GHCi Debugger
-----------------

.. index::
   single: debugger; in GHCi

GHCi contains a simple imperative-style debugger in which you can stop a
running computation in order to examine the values of variables. The
debugger is integrated into GHCi, and is turned on by default: no flags
are required to enable the debugging facilities. There is one major
restriction: breakpoints and single-stepping are only available in
interpreted modules; compiled code is invisible to the debugger [5]_.

The debugger provides the following:

-  The ability to set a breakpoint on a function definition or
   expression in the program. When the function is called, or the
   expression evaluated, GHCi suspends execution and returns to the
   prompt, where you can inspect the values of local variables before
   continuing with the execution.

-  Execution can be single-stepped: the evaluator will suspend execution
   approximately after every reduction, allowing local variables to be
   inspected. This is equivalent to setting a breakpoint at every point
   in the program.

-  Execution can take place in tracing mode, in which the evaluator
   remembers each evaluation step as it happens, but doesn't suspend
   execution until an actual breakpoint is reached. When this happens,
   the history of evaluation steps can be inspected.

-  Exceptions (e.g. pattern matching failure and ``error``) can be
   treated as breakpoints, to help locate the source of an exception in
   the program.

There is currently no support for obtaining a “stack trace”, but the
tracing and history features provide a useful second-best, which will
often be enough to establish the context of an error. For instance, it
is possible to break automatically when an exception is thrown, even if
it is thrown from within compiled code (see
:ref:`ghci-debugger-exceptions`).

.. _breakpoints:

Breakpoints and inspecting variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's use quicksort as a running example. Here's the code: ::

    qsort [] = []
    qsort (a:as) = qsort left ++ [a] ++ qsort right
      where (left,right) = (filter (<=a) as, filter (>a) as)

    main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])

First, load the module into GHCi:

.. code-block:: none

    Prelude> :l qsort.hs
    [1 of 1] Compiling Main             ( qsort.hs, interpreted )
    Ok, modules loaded: Main.
    *Main>

Now, let's set a breakpoint on the right-hand-side of the second
equation of qsort:

.. code-block:: none

    *Main> :break 2
    Breakpoint 0 activated at qsort.hs:2:15-46
    *Main>

The command ``:break 2`` sets a breakpoint on line 2 of the most
recently-loaded module, in this case ``qsort.hs``. Specifically, it
picks the leftmost complete subexpression on that line on which to set
the breakpoint, which in this case is the expression
``(qsort left ++ [a] ++ qsort right)``.

Now, we run the program:

.. code-block:: none

    *Main> main
    Stopped at qsort.hs:2:15-46
    _result :: [a]
    a :: a
    left :: [a]
    right :: [a]
    [qsort.hs:2:15-46] *Main>

Execution has stopped at the breakpoint. The prompt has changed to
indicate that we are currently stopped at a breakpoint, and the
location: ``[qsort.hs:2:15-46]``. To further clarify the location, we
can use the :ghci-cmd:`:list` command:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :list
    1  qsort [] = []
    2  qsort (a:as) = qsort left ++ [a] ++ qsort right
    3    where (left,right) = (filter (<=a) as, filter (>a) as)

The :ghci-cmd:`:list` command lists the source code around the current
breakpoint. If your output device supports it, then GHCi will highlight
the active subexpression in bold.

GHCi has provided bindings for the free variables [6]_ of the expression
on which the breakpoint was placed (``a``, ``left``, ``right``), and
additionally a binding for the result of the expression (``_result``).
These variables are just like other variables that you might define in
GHCi; you can use them in expressions that you type at the prompt, you
can ask for their types with :ghci-cmd:`:type`, and so on. There is one
important difference though: these variables may only have partial
types. For example, if we try to display the value of ``left``:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> left

    <interactive>:1:0:
        Ambiguous type variable `a' in the constraint:
          `Show a' arising from a use of `print' at <interactive>:1:0-3
        Cannot resolve unknown runtime types: a
        Use :print or :force to determine these types

This is because ``qsort`` is a polymorphic function, and because GHCi
does not carry type information at runtime, it cannot determine the
runtime types of free variables that involve type variables. Hence, when
you ask to display ``left`` at the prompt, GHCi can't figure out which
instance of ``Show`` to use, so it emits the type error above.

Fortunately, the debugger includes a generic printing command,
:ghci-cmd:`:print`, which can inspect the actual runtime value of a variable and
attempt to reconstruct its type. If we try it on ``left``:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :set -fprint-evld-with-show
    [qsort.hs:2:15-46] *Main> :print left
    left = (_t1::[a])

This isn't particularly enlightening. What happened is that ``left`` is
bound to an unevaluated computation (a suspension, or thunk), and
:ghci-cmd:`:print` does not force any evaluation. The idea is that
:ghci-cmd:`:print` can be used to inspect values at a breakpoint without any
unfortunate side effects. It won't force any evaluation, which could cause the
program to give a different answer than it would normally, and hence it won't
cause any exceptions to be raised, infinite loops, or further breakpoints to be
triggered (see :ref:`nested-breakpoints`). Rather than forcing thunks,
:ghci-cmd:`:print` binds each thunk to a fresh variable beginning with an
underscore, in this case ``_t1``.

The flag :ghc-flag:`-fprint-evld-with-show` instructs :ghci-cmd:`:print` to reuse
available ``Show`` instances when possible. This happens only when the
contents of the variable being inspected are completely evaluated.

If we aren't concerned about preserving the evaluatedness of a variable, we can
use :ghci-cmd:`:force` instead of :ghci-cmd:`:print`. The :ghci-cmd:`:force`
command behaves exactly like :ghci-cmd:`:print`, except that it forces the
evaluation of any thunks it encounters:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :force left
    left = [4,0,3,1]

Now, since :ghci-cmd:`:force` has inspected the runtime value of ``left``, it
has reconstructed its type. We can see the results of this type
reconstruction:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :show bindings
    _result :: [Integer]
    a :: Integer
    left :: [Integer]
    right :: [Integer]
    _t1 :: [Integer]

Not only do we now know the type of ``left``, but all the other partial
types have also been resolved. So we can ask for the value of ``a``, for
example:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> a
    8

You might find it useful to use Haskell's ``seq`` function to evaluate
individual thunks rather than evaluating the whole expression with
:ghci-cmd:`:force`. For example:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :print right
    right = (_t1::[Integer])
    [qsort.hs:2:15-46] *Main> seq _t1 ()
    ()
    [qsort.hs:2:15-46] *Main> :print right
    right = 23 : (_t2::[Integer])

We evaluated only the ``_t1`` thunk, revealing the head of the list, and
the tail is another thunk now bound to ``_t2``. The ``seq`` function is
a little inconvenient to use here, so you might want to use :ghci-cmd:`:def` to
make a nicer interface (left as an exercise for the reader!).

Finally, we can continue the current execution:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :continue
    Stopped at qsort.hs:2:15-46
    _result :: [a]
    a :: a
    left :: [a]
    right :: [a]
    [qsort.hs:2:15-46] *Main>

The execution continued at the point it previously stopped, and has now
stopped at the breakpoint for a second time.

.. _setting-breakpoints:

Setting breakpoints
^^^^^^^^^^^^^^^^^^^

Breakpoints can be set in various ways. Perhaps the easiest way to set a
breakpoint is to name a top-level function:

.. code-block:: none

       :break identifier

Where ⟨identifier⟩ names any top-level function in an interpreted module
currently loaded into GHCi (qualified names may be used). The breakpoint
will be set on the body of the function, when it is fully applied but
before any pattern matching has taken place.

Breakpoints can also be set by line (and optionally column) number:

.. code-block:: none

       :break line
       :break line column
       :break module line
       :break module line column

When a breakpoint is set on a particular line, GHCi sets the breakpoint
on the leftmost subexpression that begins and ends on that line. If two
complete subexpressions start at the same column, the longest one is
picked. If there is no complete subexpression on the line, then the
leftmost expression starting on the line is picked, and failing that the
rightmost expression that partially or completely covers the line.

When a breakpoint is set on a particular line and column, GHCi picks the
smallest subexpression that encloses that location on which to set the
breakpoint. Note: GHC considers the TAB character to have a width of 1,
wherever it occurs; in other words it counts characters, rather than
columns. This matches what some editors do, and doesn't match others.
The best advice is to avoid tab characters in your source code
altogether (see :ghc-flag:`-Wtabs` in :ref:`options-sanity`).

If the module is omitted, then the most recently-loaded module is used.

Not all subexpressions are potential breakpoint locations. Single
variables are typically not considered to be breakpoint locations
(unless the variable is the right-hand-side of a function definition,
lambda, or case alternative). The rule of thumb is that all redexes are
breakpoint locations, together with the bodies of functions, lambdas,
case alternatives and binding statements. There is normally no
breakpoint on a let expression, but there will always be a breakpoint on
its body, because we are usually interested in inspecting the values of
the variables bound by the let.

Listing and deleting breakpoints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The list of breakpoints currently enabled can be displayed using
:ghci-cmd:`:show breaks`:

.. code-block:: none

    *Main> :show breaks
    [0] Main qsort.hs:1:11-12
    [1] Main qsort.hs:2:15-46

To delete a breakpoint, use the :ghci-cmd:`:delete` command with the number
given in the output from :ghci-cmd:`:show breaks`:

.. code-block:: none

    *Main> :delete 0
    *Main> :show breaks
    [1] Main qsort.hs:2:15-46

To delete all breakpoints at once, use ``:delete *``.

.. _single-stepping:

Single-stepping
~~~~~~~~~~~~~~~

Single-stepping is a great way to visualise the execution of your
program, and it is also a useful tool for identifying the source of a
bug. GHCi offers two variants of stepping. Use :ghci-cmd:`:step` to enable all
the breakpoints in the program, and execute until the next breakpoint is
reached. Use :ghci-cmd:`:steplocal` to limit the set of enabled breakpoints to
those in the current top level function. Similarly, use :ghci-cmd:`:stepmodule`
to single step only on breakpoints contained in the current module. For
example:

.. code-block:: none

    *Main> :step main
    Stopped at qsort.hs:5:7-47
    _result :: IO ()

The command :ghci-cmd:`:step expr <:step>` begins the evaluation of ⟨expr⟩ in
single-stepping mode. If ⟨expr⟩ is omitted, then it single-steps from
the current breakpoint. :ghci-cmd:`:steplocal` and :ghci-cmd:`:stepmodule`
commands work similarly.

The :ghci-cmd:`:list` command is particularly useful when single-stepping, to
see where you currently are:

.. code-block:: none

    [qsort.hs:5:7-47] *Main> :list
    4
    5  main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])
    6
    [qsort.hs:5:7-47] *Main>

In fact, GHCi provides a way to run a command when a breakpoint is hit,
so we can make it automatically do :ghci-cmd:`:list`:

.. code-block:: none

    [qsort.hs:5:7-47] *Main> :set stop :list
    [qsort.hs:5:7-47] *Main> :step
    Stopped at qsort.hs:5:14-46
    _result :: [Integer]
    4
    5  main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])
    6
    [qsort.hs:5:14-46] *Main>

.. _nested-breakpoints:

Nested breakpoints
~~~~~~~~~~~~~~~~~~

When GHCi is stopped at a breakpoint, and an expression entered at the
prompt triggers a second breakpoint, the new breakpoint becomes the
"current" one, and the old one is saved on a stack. An arbitrary number
of breakpoint contexts can be built up in this way. For example:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :st qsort [1,3]
    Stopped at qsort.hs:(1,0)-(3,55)
    _result :: [a]
    ... [qsort.hs:(1,0)-(3,55)] *Main>

While stopped at the breakpoint on line 2 that we set earlier, we
started a new evaluation with ``:step qsort [1,3]``. This new evaluation
stopped after one step (at the definition of ``qsort``). The prompt has
changed, now prefixed with ``...``, to indicate that there are saved
breakpoints beyond the current one. To see the stack of contexts, use
:ghci-cmd:`:show context`:

.. code-block:: none

    ... [qsort.hs:(1,0)-(3,55)] *Main> :show context
    --> main
      Stopped at qsort.hs:2:15-46
    --> qsort [1,3]
      Stopped at qsort.hs:(1,0)-(3,55)
    ... [qsort.hs:(1,0)-(3,55)] *Main>

To abandon the current evaluation, use :ghci-cmd:`:abandon`:

.. code-block:: none

    ... [qsort.hs:(1,0)-(3,55)] *Main> :abandon
    [qsort.hs:2:15-46] *Main> :abandon
    *Main>

.. _ghci-debugger-result:

The ``_result`` variable
~~~~~~~~~~~~~~~~~~~~~~~~

When stopped at a breakpoint or single-step, GHCi binds the variable
``_result`` to the value of the currently active expression. The value
of ``_result`` is presumably not available yet, because we stopped its
evaluation, but it can be forced: if the type is known and showable,
then just entering ``_result`` at the prompt will show it. However,
there's one caveat to doing this: evaluating ``_result`` will be likely
to trigger further breakpoints, starting with the breakpoint we are
currently stopped at (if we stopped at a real breakpoint, rather than
due to :ghci-cmd:`:step`). So it will probably be necessary to issue a
:ghci-cmd:`:continue` immediately when evaluating ``_result``. Alternatively,
you can use :ghci-cmd:`:force` which ignores breakpoints.

.. _tracing:

Tracing and history
~~~~~~~~~~~~~~~~~~~

A question that we often want to ask when debugging a program is “how
did I get here?”. Traditional imperative debuggers usually provide some
kind of stack-tracing feature that lets you see the stack of active
function calls (sometimes called the “lexical call stack”), describing a
path through the code to the current location. Unfortunately this is
hard to provide in Haskell, because execution proceeds on a
demand-driven basis, rather than a depth-first basis as in strict
languages. The “stack“ in GHC's execution engine bears little
resemblance to the lexical call stack. Ideally GHCi would maintain a
separate lexical call stack in addition to the dynamic call stack, and
in fact this is exactly what our profiling system does
(:ref:`profiling`), and what some other Haskell debuggers do. For the
time being, however, GHCi doesn't maintain a lexical call stack (there
are some technical challenges to be overcome). Instead, we provide a way
to backtrack from a breakpoint to previous evaluation steps: essentially
this is like single-stepping backwards, and should in many cases provide
enough information to answer the "how did I get here?" question.

To use tracing, evaluate an expression with the :ghci-cmd:`:trace` command. For
example, if we set a breakpoint on the base case of ``qsort``:

.. code-block:: none

    *Main> :list qsort
    1  qsort [] = []
    2  qsort (a:as) = qsort left ++ [a] ++ qsort right
    3    where (left,right) = (filter (<=a) as, filter (>a) as)
    4
    *Main> :b 1
    Breakpoint 1 activated at qsort.hs:1:11-12
    *Main>

and then run a small ``qsort`` with tracing:

.. code-block:: none

    *Main> :trace qsort [3,2,1]
    Stopped at qsort.hs:1:11-12
    _result :: [a]
    [qsort.hs:1:11-12] *Main>

We can now inspect the history of evaluation steps:

.. code-block:: none

    [qsort.hs:1:11-12] *Main> :hist
    -1  : qsort.hs:3:24-38
    -2  : qsort.hs:3:23-55
    -3  : qsort.hs:(1,0)-(3,55)
    -4  : qsort.hs:2:15-24
    -5  : qsort.hs:2:15-46
    -6  : qsort.hs:3:24-38
    -7  : qsort.hs:3:23-55
    -8  : qsort.hs:(1,0)-(3,55)
    -9  : qsort.hs:2:15-24
    -10 : qsort.hs:2:15-46
    -11 : qsort.hs:3:24-38
    -12 : qsort.hs:3:23-55
    -13 : qsort.hs:(1,0)-(3,55)
    -14 : qsort.hs:2:15-24
    -15 : qsort.hs:2:15-46
    -16 : qsort.hs:(1,0)-(3,55)
    <end of history>

To examine one of the steps in the history, use :ghci-cmd:`:back`:

.. code-block:: none

    [qsort.hs:1:11-12] *Main> :back
    Logged breakpoint at qsort.hs:3:24-38
    _result :: [a]
    as :: [a]
    a :: a
    [-1: qsort.hs:3:24-38] *Main>

Note that the local variables at each step in the history have been
preserved, and can be examined as usual. Also note that the prompt has
changed to indicate that we're currently examining the first step in the
history: ``-1``. The command :ghci-cmd:`:forward` can be used to traverse
forward in the history.

The :ghci-cmd:`:trace` command can be used with or without an expression. When
used without an expression, tracing begins from the current breakpoint,
just like :ghci-cmd:`:step`.

The history is only available when using :ghci-cmd:`:trace`; the reason for this
is we found that logging each breakpoint in the history cuts performance
by a factor of 2 or more.

.. ghc-flag:: -fghci-hist-size

    :default: 50

    Modify the depth of the evaluation history tracked by GHCi.

.. _ghci-debugger-exceptions:

Debugging exceptions
~~~~~~~~~~~~~~~~~~~~

Another common question that comes up when debugging is "where did this
exception come from?". Exceptions such as those raised by ``error`` or
``head []`` have no context information attached to them. Finding which
particular call to ``head`` in your program resulted in the error can be
a painstaking process, usually involving ``Debug.Trace.trace``, or
compiling with profiling and using ``Debug.Trace.traceStack`` or
``+RTS -xc`` (see :rts-flag:`-xc`).

The GHCi debugger offers a way to hopefully shed some light on these
errors quickly and without modifying or recompiling the source code. One
way would be to set a breakpoint on the location in the source code that
throws the exception, and then use :ghci-cmd:`:trace` and :ghci-cmd:`:history` to
establish the context. However, ``head`` is in a library and we can't
set a breakpoint on it directly. For this reason, GHCi provides the
flags :ghc-flag:`-fbreak-on-exception` which causes the evaluator to stop when
an exception is thrown, and :ghc-flag:`-fbreak-on-error`, which works similarly
but stops only on uncaught exceptions. When stopping at an exception,
GHCi will act just as it does when a breakpoint is hit, with the
deviation that it will not show you any source code location. Due to
this, these commands are only really useful in conjunction with
:ghci-cmd:`:trace`, in order to log the steps leading up to the exception. For
example:

.. code-block:: none

    *Main> :set -fbreak-on-exception
    *Main> :trace qsort ("abc" ++ undefined)
    “Stopped at <exception thrown>
    _exception :: e
    [<exception thrown>] *Main> :hist
    -1  : qsort.hs:3:24-38
    -2  : qsort.hs:3:23-55
    -3  : qsort.hs:(1,0)-(3,55)
    -4  : qsort.hs:2:15-24
    -5  : qsort.hs:2:15-46
    -6  : qsort.hs:(1,0)-(3,55)
    <end of history>
    [<exception thrown>] *Main> :back
    Logged breakpoint at qsort.hs:3:24-38
    _result :: [a]
    as :: [a]
    a :: a
    [-1: qsort.hs:3:24-38] *Main> :force as
    *** Exception: Prelude.undefined
    [-1: qsort.hs:3:24-38] *Main> :print as
    as = 'b' : 'c' : (_t1::[Char])

The exception itself is bound to a new variable, ``_exception``.

Breaking on exceptions is particularly useful for finding out what your
program was doing when it was in an infinite loop. Just hit Control-C,
and examine the history to find out what was going on.

.. ghc-flag:: -fbreak-on-exception
              -fbreak-on-error

    Causes GHCi to halt evaluation and return to the interactive prompt
    in the event of an exception. While :ghc-flag:`-fbreak-on-exception` breaks
    on all exceptions, :ghc-flag:`-fbreak-on-error` breaks on only those which
    would otherwise be uncaught.

Example: inspecting functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to use the debugger to examine function values. When we
are at a breakpoint and a function is in scope, the debugger cannot show
you the source code for it; however, it is possible to get some
information by applying it to some arguments and observing the result.

The process is slightly complicated when the binding is polymorphic. We
show the process by means of an example. To keep things simple, we will
use the well known ``map`` function:

::

    import Prelude hiding (map)

    map :: (a->b) -> [a] -> [b]
    map f [] = []
    map f (x:xs) = f x : map f xs

We set a breakpoint on ``map``, and call it.

.. code-block:: none

    *Main> :break 5
    Breakpoint 0 activated at  map.hs:5:15-28
    *Main> map Just [1..5]
    Stopped at map.hs:(4,0)-(5,12)
    _result :: [b]
    x :: a
    f :: a -> b
    xs :: [a]

GHCi tells us that, among other bindings, ``f`` is in scope. However,
its type is not fully known yet, and thus it is not possible to apply it
to any arguments. Nevertheless, observe that the type of its first
argument is the same as the type of ``x``, and its result type is shared
with ``_result``.

As we demonstrated earlier (:ref:`breakpoints`), the debugger has some
intelligence built-in to update the type of ``f`` whenever the types of
``x`` or ``_result`` are discovered. So what we do in this scenario is
force ``x`` a bit, in order to recover both its type and the argument
part of ``f``.

.. code-block:: none

    *Main> seq x ()
    *Main> :print x
    x = 1

We can check now that as expected, the type of ``x`` has been
reconstructed, and with it the type of ``f`` has been too:

.. code-block:: none

    *Main> :t x
    x :: Integer
    *Main> :t f
    f :: Integer -> b

From here, we can apply f to any argument of type Integer and observe
the results.

.. code-block:: none

    *Main> let b = f 10
    *Main> :t b
    b :: b
    *Main> b
    <interactive>:1:0:
        Ambiguous type variable `b' in the constraint:
          `Show b' arising from a use of `print' at <interactive>:1:0
    *Main> :p b
    b = (_t2::a)
    *Main> seq b ()
    ()
    *Main> :t b
    b :: a
    *Main> :p b
    b = Just 10
    *Main> :t b
    b :: Maybe Integer
    *Main> :t f
    f :: Integer -> Maybe Integer
    *Main> f 20
    Just 20
    *Main> map f [1..5]
    [Just 1, Just 2, Just 3, Just 4, Just 5]

In the first application of ``f``, we had to do some more type
reconstruction in order to recover the result type of ``f``. But after
that, we are free to use ``f`` normally.

Limitations
~~~~~~~~~~~

-  When stopped at a breakpoint, if you try to evaluate a variable that
   is already under evaluation, the second evaluation will hang. The
   reason is that GHC knows the variable is under evaluation, so the new
   evaluation just waits for the result before continuing, but of course
   this isn't going to happen because the first evaluation is stopped at
   a breakpoint. Control-C can interrupt the hung evaluation and return
   to the prompt.

   The most common way this can happen is when you're evaluating a CAF
   (e.g. main), stop at a breakpoint, and ask for the value of the CAF
   at the prompt again.

-  Implicit parameters (see :ref:`implicit-parameters`) are only
   available at the scope of a breakpoint if there is an explicit type
   signature.

.. _ghci-invocation:

Invoking GHCi
-------------

.. index::
   single: invoking; GHCi
   single: --interactive

GHCi is invoked with the command ``ghci`` or ``ghc --interactive``. One
or more modules or filenames can also be specified on the command line;
this instructs GHCi to load the specified modules or filenames (and all
the modules they depend on), just as if you had said ``:load modules``
at the GHCi prompt (see :ref:`ghci-commands`). For example, to start
GHCi and load the program whose topmost module is in the file
``Main.hs``, we could say:

.. code-block:: none

    $ ghci Main.hs

Most of the command-line options accepted by GHC (see :ref:`using-ghc`)
also make sense in interactive mode. The ones that don't make sense are
mostly obvious.

.. ghc-flag:: -flocal-ghci-history

  By default, GHCi keeps global history in ``~/.ghc/ghci_history`` or
  ``%APPDATA%/<app>/ghci_history``, but you can use current directory, e.g.:

  .. code-block:: none

      $ ghci -flocal-ghci-history

  It will create ``.ghci-history`` in current folder where GHCi is launched.

Packages
~~~~~~~~

.. index::
   single: packages; with GHCi

Most packages (see :ref:`using-packages`) are available without needing
to specify any extra flags at all: they will be automatically loaded the
first time they are needed.

For hidden packages, however, you need to request the package be loaded
by using the :ghc-flag:`-package` flag:

.. code-block:: none

    $ ghci -package readline
    GHCi, version 6.8.1: http://www.haskell.org/ghc/  :? for help
    Loading package base ... linking ... done.
    Loading package readline-1.0 ... linking ... done.
    Prelude>

The following command works to load new packages into a running GHCi:

.. code-block:: none

    Prelude> :set -package name

But note that doing this will cause all currently loaded modules to be
unloaded, and you'll be dumped back into the ``Prelude``.

Extra libraries
~~~~~~~~~~~~~~~

.. index::
   single: libraries; with GHCi

Extra libraries may be specified on the command line using the normal
``-llib`` option. (The term *library* here refers to libraries of
foreign object code; for using libraries of Haskell source code, see
:ref:`ghci-modules-filenames`.) For example, to load the “m” library:

.. code-block:: none

    $ ghci -lm

On systems with ``.so``-style shared libraries, the actual library
loaded will the ``liblib.so``. GHCi searches the following places for
libraries, in this order:

-  Paths specified using the :ghc-flag:`-L` command-line option,

-  the standard library search path for your system, which on some
   systems may be overridden by setting the :envvar:`LD_LIBRARY_PATH`
   environment variable.

On systems with ``.dll``-style shared libraries, the actual library
loaded will be ``lib.dll``. Again, GHCi will signal an error if it can't
find the library.

GHCi can also load plain object files (``.o`` or ``.obj`` depending on
your platform) from the command-line. Just add the name the object file
to the command line.

Ordering of ``-l`` options matters: a library should be mentioned
*before* the libraries it depends on (see :ref:`options-linker`).

.. _ghci-commands:

GHCi commands
-------------

GHCi commands all begin with "``:``" and consist of a single command
name followed by zero or more parameters. The command name may be
abbreviated, with ambiguities being resolved in favour of the more
commonly used commands.

.. comment

    This section makes use of the GHC-specific :directive:`ghci-cmd` directive
    defined in :file:`conf.py`. This is used to describe and cross-reference GHCi
    commands.


.. ghci-cmd:: :abandon

    Abandons the current evaluation (only available when stopped at a
    breakpoint).

.. ghci-cmd:: :add;[*] ⟨module⟩

    Add ⟨module⟩(s) to the current target set, and perform a reload.
    Normally pre-compiled code for the module will be loaded if
    available, or otherwise the module will be compiled to byte-code.
    Using the ``*`` prefix forces the module to be loaded as byte-code.

.. ghci-cmd:: :all-types

    List all types collected for expressions and (local) bindings
    currently loaded (while :ghci-cmd:`:set +c` was active) with their respective
    source-code span, e.g. ::

       GhciTypes> :all-types
       GhciTypes.hs:(38,13)-(38,24): Maybe Id
       GhciTypes.hs:(45,10)-(45,29): Outputable SpanInfo
       GhciTypes.hs:(45,10)-(45,29): (Rational -> SpanInfo -> SDoc) -> Outputable SpanInfo

.. ghci-cmd:: :back; ⟨n⟩

    Travel back ⟨n⟩ steps in the history. ⟨n⟩ is one if omitted. See
    :ref:`tracing` for more about GHCi's debugging facilities. See also:
    :ghci-cmd:`:trace`, :ghci-cmd:`:history`, :ghci-cmd:`:forward`.

.. ghci-cmd:: :break; [⟨identifier⟩ | [⟨module⟩] ⟨line⟩ [⟨column⟩]]

    Set a breakpoint on the specified function or line and column. See
    :ref:`setting-breakpoints`.

.. ghci-cmd:: :browse;[!] [[*] ⟨module⟩]

    Displays the identifiers exported by the module ⟨module⟩, which must
    be either loaded into GHCi or be a member of a package. If ⟨module⟩
    is omitted, the most recently-loaded module is used.

    Like all other GHCi commands, the output is always displayed in the
    current GHCi scope (:ref:`ghci-scope`).

    There are two variants of the browse command:

    -  If the ``*`` symbol is placed before the module name, then *all*
       the identifiers in scope in ⟨module⟩ (rather that just its
       exports) are shown.

       The ``*``-form is only available for modules which are
       interpreted; for compiled modules (including modules from
       packages) only the non-\ ``*`` form of :ghci-cmd:`:browse` is available.

    -  Data constructors and class methods are usually displayed in the
       context of their data type or class declaration. However, if the
       ``!`` symbol is appended to the command, thus ``:browse!``, they
       are listed individually. The ``!``-form also annotates the
       listing with comments giving possible imports for each group of
       entries. Here is an example: ::

           Prelude> :browse! Data.Maybe
           -- not currently imported
           Data.Maybe.catMaybes :: [Maybe a] -> [a]
           Data.Maybe.fromJust :: Maybe a -> a
           Data.Maybe.fromMaybe :: a -> Maybe a -> a
           Data.Maybe.isJust :: Maybe a -> Bool
           Data.Maybe.isNothing :: Maybe a -> Bool
           Data.Maybe.listToMaybe :: [a] -> Maybe a
           Data.Maybe.mapMaybe :: (a -> Maybe b) -> [a] -> [b]
           Data.Maybe.maybeToList :: Maybe a -> [a]
           -- imported via Prelude
           Just :: a -> Maybe a
           data Maybe a = Nothing | Just a
           Nothing :: Maybe a
           maybe :: b -> (a -> b) -> Maybe a -> b

       This output shows that, in the context of the current session (ie
       in the scope of ``Prelude``), the first group of items from
       ``Data.Maybe`` are not in scope (althought they are available in
       fully qualified form in the GHCi session - see
       :ref:`ghci-scope`), whereas the second group of items are in
       scope (via ``Prelude``) and are therefore available either
       unqualified, or with a ``Prelude.`` qualifier.

.. ghci-cmd:: :cd; ⟨dir⟩

    Changes the current working directory to ⟨dir⟩. A "``~``" symbol
    at the beginning of ⟨dir⟩ will be replaced by the contents of the
    environment variable :envvar:`HOME`. See also the :ghci-cmd:`:show paths`
    command for showing the current working directory.

    Note: changing directories causes all currently loaded modules to be
    unloaded. This is because the search path is usually expressed using
    relative directories, and changing the search path in the middle of
    a session is not supported.

.. ghci-cmd:: :cmd; ⟨expr⟩

    Executes ⟨expr⟩ as a computation of type ``IO String``, and then
    executes the resulting string as a list of GHCi commands. Multiple
    commands are separated by newlines. The :ghci-cmd:`:cmd` command is useful
    with :ghci-cmd:`:def` and :ghci-cmd:`:set stop`.

.. ghci-cmd:: :complete; ⟨type⟩ [⟨n⟩-][⟨m⟩] ⟨string-literal⟩

    This command allows to request command completions from GHCi even
    when interacting over a pipe instead of a proper terminal and is
    designed for integrating GHCi's completion with text editors and
    IDEs.

    When called, :ghci-cmd:`:complete` prints the ⟨n⟩\ :sup:`th` to
    ⟨m⟩\ :sup:`th` completion candidates for the partial input
    ⟨string-literal⟩ for the completion domain denoted by ⟨type⟩.
    Currently, only the ``repl`` domain is supported which denotes the
    kind of completion that would be provided interactively by GHCi at
    the input prompt.

    If omitted, ⟨n⟩ and ⟨m⟩ default to the first or last available
    completion candidate respectively. If there are less candidates than
    requested via the range argument, ⟨n⟩ and ⟨m⟩ are implicitly capped
    to the number of available completition candidates.

    The output of :ghci-cmd:`:complete` begins with a header line containing
    three space-delimited fields:

    -  An integer denoting the number ``l`` of printed completions,
    -  an integer denoting the total number of completions available,
       and finally
    -  a string literal denoting a common prefix to be added to the
       returned completion candidates.

    The header line is followed by ⟨l⟩ lines each containing one
    completion candidate encoded as (quoted) string literal. Here are
    some example invocations showing the various cases:

    .. code-block:: none

        Prelude> :complete repl 0 ""
        0 470 ""
        Prelude> :complete repl 5 "import For"
        5 21 "import "
        "Foreign"
        "Foreign.C"
        "Foreign.C.Error"
        "Foreign.C.String"
        "Foreign.C.Types"
        Prelude> :complete repl 5-10 "import For"
        6 21 "import "
        "Foreign.C.Types"
        "Foreign.Concurrent"
        "Foreign.ForeignPtr"
        "Foreign.ForeignPtr.Safe"
        "Foreign.ForeignPtr.Unsafe"
        "Foreign.Marshal"
        Prelude> :complete repl 20- "import For"
        2 21 "import "
        "Foreign.StablePtr"
        "Foreign.Storable"
        Prelude> :complete repl "map"
        3 3 ""
        "map"
        "mapM"
        "mapM_"
        Prelude> :complete repl 5-10 "map"
        0 3 ""

.. ghci-cmd:: :continue

    Continue the current evaluation, when stopped at a breakpoint.

.. ghci-cmd:: :ctags; [⟨filename⟩]

    Generates a "tags" file for Vi-style editors (:ghci-cmd:`:ctags`) or
    Emacs-style editors (:ghci-cmd:`:etags`). If no filename is specified, the
    default ``tags`` or ``TAGS`` is used, respectively. Tags for all the
    functions, constructors and types in the currently loaded modules
    are created. All modules must be interpreted for these commands to
    work.

.. ghci-cmd:: :def;[!] ⟨name⟩ ⟨expr⟩

    :ghci-cmd:`:def` is used to define new commands, or macros, in GHCi. The
    command ``:def ⟨name⟩ ⟨expr⟩`` defines a new GHCi command ``:name``,
    implemented by the Haskell expression ⟨expr⟩, which must have type
    ``String -> IO String``. When ``:name args`` is typed at the prompt,
    GHCi will run the expression ``(name args)``, take the resulting
    ``String``, and feed it back into GHCi as a new sequence of
    commands. Separate commands in the result must be separated by
    "``\n``".

    That's all a little confusing, so here's a few examples. To start
    with, here's a new GHCi command which doesn't take any arguments or
    produce any results, it just outputs the current date and time:

    .. code-block:: none

        Prelude> let date _ = Time.getClockTime >>= print >> return ""
        Prelude> :def date date
        Prelude> :date
        Fri Mar 23 15:16:40 GMT 2001

    Here's an example of a command that takes an argument. It's a
    re-implementation of :ghci-cmd:`:cd`:

    .. code-block:: none

        Prelude> let mycd d = Directory.setCurrentDirectory d >> return ""
        Prelude> :def mycd mycd
        Prelude> :mycd ..

    Or I could define a simple way to invoke "``ghc --make Main``"
    in the current directory:

    .. code-block:: none

        Prelude> :def make (\_ -> return ":! ghc --make Main")

    We can define a command that reads GHCi input from a file. This
    might be useful for creating a set of bindings that we want to
    repeatedly load into the GHCi session:

    .. code-block:: none

        Prelude> :def . readFile
        Prelude> :. cmds.ghci

    Notice that we named the command ``:.``, by analogy with the
    "``.``" Unix shell command that does the same thing.

    Typing ``:def`` on its own lists the currently-defined macros.
    Attempting to redefine an existing command name results in an error
    unless the ``:def!`` form is used, in which case the old command
    with that name is silently overwritten.

.. ghci-cmd:: :delete; * | ⟨num⟩ ...

    Delete one or more breakpoints by number (use :ghci-cmd:`:show breaks` to
    see the number of each breakpoint). The ``*`` form deletes all the
    breakpoints.

.. ghci-cmd:: :edit; ⟨file⟩

    Opens an editor to edit the file ⟨file⟩, or the most recently loaded
    module if ⟨file⟩ is omitted. If there were errors during the last
    loading, the cursor will be positioned at the line of the first
    error. The editor to invoke is taken from the :envvar:`EDITOR` environment
    variable, or a default editor on your system if :envvar:`EDITOR` is not
    set. You can change the editor using :ghci-cmd:`:set editor`.

.. ghci-cmd:: :etags

    See :ghci-cmd:`:ctags`.

.. ghci-cmd:: :force; ⟨identifier⟩ ...

    Prints the value of ⟨identifier⟩ in the same way as :ghci-cmd:`:print`.
    Unlike :ghci-cmd:`:print`, :ghci-cmd:`:force` evaluates each thunk that it
    encounters while traversing the value. This may cause exceptions or
    infinite loops, or further breakpoints (which are ignored, but
    displayed).

.. ghci-cmd:: :forward; ⟨n⟩

    Move forward ⟨n⟩ steps in the history. ⟨n⟩ is one if omitted. See
    :ref:`tracing` for more about GHCi's debugging facilities. See also:
    :ghci-cmd:`:trace`, :ghci-cmd:`:history`, :ghci-cmd:`:back`.

.. ghci-cmd:: :help
              :?

    Displays a list of the available commands.

.. ghci-cmd:: :

    .. index::
       pair: Repeating last command; in GHCi

    Repeat the previous command.

.. ghci-cmd:: :history; [num]

    Display the history of evaluation steps. With a number, displays
    that many steps (default: 20). For use with :ghci-cmd:`:trace`; see
    :ref:`tracing`. To set the number of history entries stored by GHCi,
    use the :ghc-flag:`-fghci-hist-size` flag.

.. ghci-cmd:: :info;[!] ⟨name⟩

    Displays information about the given name(s). For example, if ⟨name⟩
    is a class, then the class methods and their types will be printed;
    if ⟨name⟩ is a type constructor, then its definition will be
    printed; if ⟨name⟩ is a function, then its type will be printed. If
    ⟨name⟩ has been loaded from a source file, then GHCi will also
    display the location of its definition in the source.

    For types and classes, GHCi also summarises instances that mention
    them. To avoid showing irrelevant information, an instance is shown
    only if (a) its head mentions ⟨name⟩, and (b) all the other things
    mentioned in the instance are in scope (either qualified or
    otherwise) as a result of a :ghci-cmd:`:load` or :ghci-cmd:`:module`
    commands.

    The command ``:info!`` works in a similar fashion but it removes
    restriction (b), showing all instances that are in scope and mention
    ⟨name⟩ in their head.

.. ghci-cmd:: :issafe; [⟨module⟩]

    Displays Safe Haskell information about the given module (or the
    current module if omitted). This includes the trust type of the
    module and its containing package.

.. ghci-cmd:: :kind;[!] ⟨type⟩

    Infers and prints the kind of ⟨type⟩. The latter can be an arbitrary
    type expression, including a partial application of a type
    constructor, such as ``Either Int``. In fact, :ghci-cmd:`:kind` even allows
    you to write a partial application of a type synonym (usually
    disallowed), so that this works:

    .. code-block:: none

        ghci> type T a b = (a,b,a)
        ghci> :k T Int Bool
        T Int Bool :: *
        ghci> :k T
        T :: * -> * -> *
        ghci> :k T Int
        T Int :: * -> *

    If you specify the optional "``!``", GHC will in addition normalise
    the type by expanding out type synonyms and evaluating type-function
    applications, and display the normalised result.

.. ghci-cmd:: :list; ⟨identifier⟩

    Lists the source code around the definition of ⟨identifier⟩ or the
    current breakpoint if not given. This requires that the identifier
    be defined in an interpreted module. If your output device supports
    it, then GHCi will highlight the active subexpression in bold.

.. ghci-cmd:: :list [⟨module⟩]; ⟨line⟩

    Lists the source code around the given line number of ⟨module⟩. This
    requires that the module be interpreted. If your output device
    supports it, then GHCi will highlight the active subexpression in
    bold.

.. ghci-cmd:: :load;[!] [*]⟨module⟩

    Recursively loads the specified ⟨module⟩s, and all the modules they
    depend on. Here, each ⟨module⟩ must be a module name or filename,
    but may not be the name of a module in a package.

    All previously loaded modules, except package modules, are
    forgotten. The new set of modules is known as the target set. Note
    that :ghci-cmd:`:load` can be used without any arguments to unload all the
    currently loaded modules and bindings.

    Normally pre-compiled code for a module will be loaded if available,
    or otherwise the module will be compiled to byte-code. Using the
    ``*`` prefix forces a module to be loaded as byte-code.

    Adding the optional "``!``" turns type errors into warnings while
    loading. This allows to use the portions of the module that are
    correct, even if there are type errors in some definitions.
    Effectively, the "-fdefer-type-errors" flag is set before loading
    and unset after loading if the flag has not already been set before.
    See :ref:`defer-type-errors` for further motivation and details.

    After a :ghci-cmd:`:load` command, the current context is set to:

    -  ⟨module⟩, if it was loaded successfully, or

    -  the most recently successfully loaded module, if any other
       modules were loaded as a result of the current :ghci-cmd:`:load`, or

    -  ``Prelude`` otherwise.

.. ghci-cmd:: :loc-at; ⟨module⟩ ⟨line⟩ ⟨col⟩ ⟨end-line⟩ ⟨end-col⟩ [⟨name⟩]

    Tries to find the definition site of the name at the given
    source-code span, e.g.:

    .. code-block:: none

        X> :loc-at X.hs 6 14 6 16 mu
        X.hs:(8,7)-(8,9)

    This command is useful when integrating GHCi with text editors and
    IDEs for providing a goto-definition facility.

    The ``:loc-at`` command requires :ghci-cmd:`:set +c` to be set.

.. ghci-cmd:: :main; ⟨arg1⟩ ... ⟨argn⟩

    When a program is compiled and executed, it can use the ``getArgs``
    function to access the command-line arguments. However, we cannot
    simply pass the arguments to the ``main`` function while we are
    testing in ghci, as the ``main`` function doesn't take its arguments
    directly.

    Instead, we can use the :ghci-cmd:`:main` command. This runs whatever
    ``main`` is in scope, with any arguments being treated the same as
    command-line arguments, e.g.:

    .. code-block:: none

        Prelude> main = System.Environment.getArgs >>= print
        Prelude> :main foo bar
        ["foo","bar"]

    We can also quote arguments which contains characters like spaces,
    and they are treated like Haskell strings, or we can just use
    Haskell list syntax:

    .. code-block:: none

        Prelude> :main foo "bar baz"
        ["foo","bar baz"]
        Prelude> :main ["foo", "bar baz"]
        ["foo","bar baz"]

    Finally, other functions can be called, either with the ``-main-is``
    flag or the :ghci-cmd:`:run` command:

    .. code-block:: none

        Prelude> foo = putStrLn "foo" >> System.Environment.getArgs >>= print
        Prelude> bar = putStrLn "bar" >> System.Environment.getArgs >>= print
        Prelude> :set -main-is foo
        Prelude> :main foo "bar baz"
        foo
        ["foo","bar baz"]
        Prelude> :run bar ["foo", "bar baz"]
        bar
        ["foo","bar baz"]

.. ghci-cmd:: :module; +|- [*]⟨mod1⟩ ...
.. ghci-cmd:: import; ⟨mod⟩

    Sets or modifies the current context for statements typed at the
    prompt. The form ``import mod`` is equivalent to ``:module +mod``.
    See :ref:`ghci-scope` for more details.

.. ghci-cmd:: :print; ⟨names⟩

    Prints a value without forcing its evaluation. :ghci-cmd:`:print` may be
    used on values whose types are unknown or partially known, which
    might be the case for local variables with polymorphic types at a
    breakpoint. While inspecting the runtime value, :ghci-cmd:`:print` attempts
    to reconstruct the type of the value, and will elaborate the type in
    GHCi's environment if possible. If any unevaluated components
    (thunks) are encountered, then :ghci-cmd:`:print` binds a fresh variable
    with a name beginning with ``_t`` to each thunk. See
    :ref:`breakpoints` for more information. See also the :ghci-cmd:`:sprint`
    command, which works like :ghci-cmd:`:print` but does not bind new
    variables.

.. ghci-cmd:: :quit

    Quits GHCi. You can also quit by typing :kbd:`Control-D` at the prompt.

.. ghci-cmd:: :reload;[!]

    Attempts to reload the current target set (see :ghci-cmd:`:load`) if any of
    the modules in the set, or any dependent module, has changed. Note
    that this may entail loading new modules, or dropping modules which
    are no longer indirectly required by the target.

    Adding the optional "``!``" turns type errors into warnings while
    loading. This allows to use the portions of the module that are
    correct, even if there are type errors in some definitions.
    Effectively, the "-fdefer-type-errors" flag is set before loading
    and unset after loading if the flag has not already been set before.
    See :ref:`defer-type-errors` for further motivation and details.

.. ghci-cmd:: :run

    See :ghci-cmd:`:main`.

.. ghci-cmd:: :script; [⟨n⟩] ⟨filename⟩

    Executes the lines of a file as a series of GHCi commands. This
    command is compatible with multiline statements as set by
    :ghci-cmd:`:set +m`

.. ghci-cmd:: :set; [⟨option⟩ ...]

    Sets various options. See :ref:`ghci-set` for a list of available
    options and :ref:`interactive-mode-options` for a list of
    GHCi-specific flags. The :ghci-cmd:`:set` command by itself shows which
    options are currently set. It also lists the current dynamic flag
    settings, with GHCi-specific flags listed separately.

.. ghci-cmd:: :set args; ⟨arg⟩

    .. index::
       single: getArgs, behavior in GHCi

    Sets the list of arguments which are returned when the program calls
    ``System.getArgs``.

.. ghci-cmd:: :set editor; ⟨cmd⟩

    Sets the command used by :ghci-cmd:`:edit` to ⟨cmd⟩.

.. ghci-cmd:: :set prog; ⟨prog⟩

    .. index::
       single: getProgName, behavior in GHCi

    Sets the string to be returned when the program calls
    ``System.getProgName``.

.. ghci-cmd:: :set prompt; ⟨prompt⟩

    .. index::
       single: GHCi prompt; setting

    Sets the string to be used as the prompt in GHCi. Inside ⟨prompt⟩,
    the next sequences are replaced:

    - ``%s`` by the names of the modules currently in scope.
    - ``%l`` by the line number (as referenced in compiler messages) of the
      current prompt.
    - ``%d`` by the date in "Weekday Month Date" format (e.g., "Tue May 26") .
    - ``%t`` by the current time in 24-hour HH:MM:SS format.
    - ``%T`` by the current time in 12-hour HH:MM:SS format.
    - ``%@`` by the current time in 12-hour am/pm format.
    - ``%A`` by the current time in 24-hour HH:MM format.
    - ``%u`` by the username of the current user.
    - ``%w`` by the current working directory.
    - ``%o`` by the operating system.
    - ``%a`` by the machine architecture.
    - ``%N`` by the compiler name.
    - ``%V`` by the compiler version.
    - ``%call(cmd [args])`` by the result of calling ``cmd args``.
    - ``%%`` by ``%``.

    If ⟨prompt⟩ starts with ``"`` then it is parsed as a Haskell String;
    otherwise it is treated as a literal string.

.. ghci-cmd:: :set prompt-cont; ⟨prompt⟩

    Sets the string to be used as the continuation prompt (used when
    using the :ghci-cmd:`:{` command) in GHCi.

.. ghci-cmd:: :set prompt-function; <prompt-function>

    .. index::
       single: GHCi prompt function; setting

    Sets the function to be used for the prompt displaying in GHCi. The
    function should be of the type ``[String] -> Int -> IO String``. This
    function is called each time the prompt is being made. The first argument
    stands for the names of the modules currently in scope(the name of the
    "topmost" module  will begin with a ``*``; see  :ref:`ghci-scope` for
    more information). The second arguments is the line number (as referenced
    in compiler  messages) of the current prompt.

.. ghci-cmd:: :set prompt-cont-function; <prompt-function>

   Sets the function to be used for the continuation prompt (used when
   using the :ghci-cmd:`:{` command) displaying in GHCi.

.. ghci-cmd:: :set stop; ⟨num⟩ ⟨cmd⟩

    Set a command to be executed when a breakpoint is hit, or a new item
    in the history is selected. The most common use of :ghci-cmd:`:set stop` is
    to display the source code at the current location, e.g.
    ``:set stop :list``.

    If a number is given before the command, then the commands are run
    when the specified breakpoint (only) is hit. This can be quite
    useful: for example, ``:set stop 1 :continue`` effectively disables
    breakpoint 1, by running :ghci-cmd:`:continue` whenever it is hit (although
    GHCi will still emit a message to say the breakpoint was hit). What's more,
    with cunning use of :ghci-cmd:`:def` and :ghci-cmd:`:cmd` you can use
    :ghci-cmd:`:set stop` to implement conditional breakpoints:

    .. code-block:: none

        *Main> :def cond \expr -> return (":cmd if (" ++ expr ++ ") then return \"\" else return \":continue\"")
        *Main> :set stop 0 :cond (x < 3)

    Ignoring breakpoints for a specified number of iterations is also
    possible using similar techniques.

.. ghci-cmd:: :seti; [⟨option⟩ ...]

    Like :ghci-cmd:`:set`, but options set with :ghci-cmd:`:seti` affect only
    expressions and commands typed at the prompt, and not modules loaded
    with :ghci-cmd:`:load` (in contrast, options set with :ghci-cmd:`:set` apply
    everywhere). See :ref:`ghci-interactive-options`.

    Without any arguments, displays the current set of options that are
    applied to expressions and commands typed at the prompt.

.. ghci-cmd:: :show bindings

    Show the bindings made at the prompt and their types.

.. ghci-cmd:: :show breaks

    List the active breakpoints.

.. ghci-cmd:: :show context

    List the active evaluations that are stopped at breakpoints.

.. ghci-cmd:: :show imports

    Show the imports that are currently in force, as created by
    ``import`` and :ghci-cmd:`:module` commands.

.. ghci-cmd:: :show modules

    Show the list of modules currently loaded.

.. ghci-cmd:: :show packages

    Show the currently active package flags, as well as the list of
    packages currently loaded.

.. ghci-cmd:: :show paths

    Show the current working directory (as set via :ghci-cmd:`:cd` command), as
    well as the list of directories searched for source files (as set by the
    ``-i`` option).

.. ghci-cmd:: :show language

    Show the currently active language flags for source files.

.. ghci-cmd:: :showi language

    Show the currently active language flags for expressions typed at
    the prompt (see also :ghci-cmd:`:seti`).

.. ghci-cmd:: :show; [args|prog|prompt|editor|stop]

    Displays the specified setting (see :ghci-cmd:`:set`).

.. ghci-cmd:: :sprint; ⟨expr⟩

    Prints a value without forcing its evaluation. :ghci-cmd:`:sprint` is
    similar to :ghci-cmd:`:print`, with the difference that unevaluated subterms
    are not bound to new variables, they are simply denoted by ``_``.

.. ghci-cmd:: :step; [⟨expr⟩]

    Enable all breakpoints and begin evaluating an expression in
    single-stepping mode. In this mode evaluation will be stopped after
    every reduction, allowing local variables to be inspected. If ⟨expr⟩
    is not given, evaluation will resume at the last breakpoint. See
    :ref:`single-stepping`.

.. ghci-cmd:: :steplocal

    Enable only breakpoints in the current top-level binding and resume
    evaluation at the last breakpoint.

.. ghci-cmd:: :stepmodule

    Enable only breakpoints in the current module and resume evaluation
    at the last breakpoint.

.. ghci-cmd:: :trace; ⟨expr⟩

    Evaluates the given expression (or from the last breakpoint if no
    expression is given), and additionally logs the evaluation steps for
    later inspection using :ghci-cmd:`:history`. See :ref:`tracing`.

.. ghci-cmd:: :type; ⟨expression⟩

    Infers and prints the type of ⟨expression⟩, including explicit
    forall quantifiers for polymorphic types.
    The type reported is the type that would be inferred
    for a variable assigned to the expression, but without the
    monomorphism restriction applied.

    .. code-block:: none

	*X> :type length
	length :: Foldable t => t a -> Int

.. ghci-cmd:: :type +v ⟨expression⟩

    Infers and prints the type of ⟨expression⟩, but without fiddling
    with type variables or class constraints. This is useful when you
    are using :ghc-flag:`-XTypeApplications` and care about the distinction
    between specified type variables (available for type application)
    and inferred type variables (not available). This mode sometimes prints
    constraints (such as ``Show Int``) that could readily be solved, but
    solving these constraints may affect the type variables, so GHC refrains.

    .. code-block:: none

	*X> :set -fprint-explicit-foralls
	*X> :type +v length
	length :: forall (t :: * -> *). Foldable t => forall a. t a -> Int

.. ghci-cmd:: :type +d ⟨expression⟩

    Infers and prints the type of ⟨expression⟩, defaulting type variables
    if possible. In this mode, if the inferred type is constrained by
    any interactive class (``Num``, ``Show``, ``Eq``, ``Ord``, ``Foldable``,
    or ``Traversable``), the constrained type variable(s) are defaulted
    according to the rules described under :ghc-flag:`-XExtendedDefaultRules`.
    This mode is quite useful when the inferred type is quite general (such
    as for ``foldr``) and it may be helpful to see a more concrete
    instantiation.

    .. code-block:: none

	*X> :type +d length
	length :: [a] -> Int

.. ghci-cmd:: :type-at; ⟨module⟩ ⟨line⟩ ⟨col⟩ ⟨end-line⟩ ⟨end-col⟩ [⟨name⟩]

    Reports the inferred type at the given span/position in the module, e.g.:

    .. code-block:: none

       *X> :type-at X.hs 6 6 6 7 f
       Int -> Int

    This command is useful when integrating GHCi with text editors and
    IDEs for providing a show-type-under-point facility.

    The last string parameter is useful for when the span is out of
    date, i.e. the file changed and the code has moved. In which case
    :ghci-cmd:`:type-at` falls back to a general :ghci-cmd:`:type` like lookup.

    The :ghci-cmd:`:type-at` command requires :ghci-cmd:`:set +c` to be set.

.. ghci-cmd:: :undef; ⟨name⟩

    Undefines the user-defined command ⟨name⟩ (see :ghci-cmd:`:def` above).

.. ghci-cmd:: :unset; ⟨option⟩

    Unsets certain options. See :ref:`ghci-set` for a list of available
    options.

.. ghci-cmd:: :uses; ⟨module⟩ ⟨line⟩ ⟨col⟩ ⟨end-line⟩ ⟨end-col⟩ [⟨name⟩]

    Reports all module-local uses of the thing at the given position
    in the module, e.g.:

    .. code-block:: none

       :uses GhciFind.hs 53 66 53 70 name
       GhciFind.hs:(46,25)-(46,29)
       GhciFind.hs:(47,37)-(47,41)
       GhciFind.hs:(53,66)-(53,70)
       GhciFind.hs:(57,62)-(57,66)

    This command is useful for highlighting and navigating all uses of
    an identifier in editors and IDEs.

    The :ghci-cmd:`:uses` command requires :ghci-cmd:`:set +c` to be set.

.. ghci-cmd:: :! ⟨command⟩

    .. index::
       single: shell commands; in GHCi

    Executes the shell command ⟨command⟩.


.. _ghci-set:

The ``:set`` and ``:seti`` commands
-----------------------------------

.. index::
   single: :set; command in GHCi
   single: :seti

The :ghci-cmd:`:set` command sets two types of options: GHCi options, which
begin with "``+``", and "command-line" options, which begin with "``-``".

.. note::
    At the moment, the :ghci-cmd:`:set` command doesn't support any kind of
    quoting in its arguments: quotes will not be removed and cannot be used
    to group words together. For example, ``:set -DFOO='BAR BAZ'`` will not
    do what you expect.

GHCi options
~~~~~~~~~~~~

.. index::
   single: options; GHCi

GHCi options may be set using :ghci-cmd:`:set` and unset using :ghci-cmd:`:unset`.

The available GHCi options are:

.. ghci-cmd:: :set +c

    Collect type and location information after loading modules.
    The commands :ghci-cmd:`:all-types`, :ghci-cmd:`:loc-at`,
    :ghci-cmd:`:type-at`, and :ghci-cmd:`:uses` require ``+c`` to be active.

.. ghci-cmd:: :set +m

    .. index::
       single: multiline input; in GHCi

    Enable parsing of multiline commands. A multiline command is
    prompted for when the current input line contains open layout
    contexts (see :ref:`ghci-multiline`).

.. ghci-cmd:: :set +r

    .. index::
       single: CAFs; in GHCi
       single: Constant Applicative Form

    Normally, any evaluation of top-level expressions (otherwise known
    as CAFs or Constant Applicative Forms) in loaded modules is retained
    between evaluations. Turning on ``+r`` causes all evaluation of
    top-level expressions to be discarded after each evaluation (they
    are still retained *during* a single evaluation).

    This option may help if the evaluated top-level expressions are
    consuming large amounts of space, or if you need repeatable
    performance measurements.

.. ghci-cmd:: :set +s

    Display some stats after evaluating each expression, including the
    elapsed time and number of bytes allocated. NOTE: the allocation
    figure is only accurate to the size of the storage manager's
    allocation area, because it is calculated at every GC. Hence, you
    might see values of zero if no GC has occurred.

.. ghci-cmd:: :set +t

    .. index::
       single: displaying type; in GHCi

    Display the type of each variable bound after a statement is entered
    at the prompt. If the statement is a single expression, then the
    only variable binding will be for the variable ``it``.

.. _ghci-cmd-line-options:

Setting GHC command-line options in GHCi
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Normal GHC command-line options may also be set using :ghci-cmd:`:set`. For
example, to turn on :ghc-flag:`-Wmissing-signatures`, you would say:

.. code-block:: none

    Prelude> :set -Wmissing-signatures

Any GHC command-line option that is designated as dynamic (see the table
in :ref:`flag-reference`), may be set using :ghci-cmd:`:set`. To unset an
option, you can set the reverse option:

.. index::
   single: dynamic; options

.. code-block:: none

    Prelude> :set -Wno-incomplete-patterns -XNoMultiParamTypeClasses

:ref:`flag-reference` lists the reverse for each option where
applicable.

Certain static options (:ghc-flag:`-package`, :ghc-flag:`-I`, :ghc-flag:`-i`,
and :ghc-flag:`-l` in particular) will also work, but some may not take effect
until the next reload.

.. index::
   single: static; options

.. _ghci-interactive-options:

Setting options for interactive evaluation only
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHCi actually maintains *two* sets of options:

-  The *loading options* apply when loading modules

-  The *interactive options* apply when evaluating expressions and
   commands typed at the GHCi prompt.

The :ghci-cmd:`:set` command modifies both, but there is also a
:ghci-cmd:`:seti` command (for "set interactive") that affects only the
interactive options set.

It is often useful to change the interactive options, without having
that option apply to loaded modules too. For example

.. code-block:: none

    :seti -XMonoLocalBinds

It would be undesirable if :ghc-flag:`-XMonoLocalBinds` were to apply to loaded
modules too: that might cause a compilation error, but more commonly it
will cause extra recompilation, because GHC will think that it needs to
recompile the module because the flags have changed.

If you are setting language options in your ``.ghci`` file, it is good
practice to use :ghci-cmd:`:seti` rather than :ghci-cmd:`:set`, unless you
really do want them to apply to all modules you load in GHCi.

The two sets of options can be inspected using the :ghci-cmd:`:set` and
:ghci-cmd:`:seti` commands respectively, with no arguments. For example, in a
clean GHCi session we might see something like this:

.. code-block:: none

    Prelude> :seti
    base language is: Haskell2010
    with the following modifiers:
      -XNoMonomorphismRestriction
      -XNoDatatypeContexts
      -XNondecreasingIndentation
      -XExtendedDefaultRules
    GHCi-specific dynamic flag settings:
    other dynamic, non-language, flag settings:
      -fimplicit-import-qualified
    warning settings:

The two sets of options are initialised as follows. First, both sets of
options are initialised as described in :ref:`ghci-dot-files`. Then the
interactive options are modified as follows:

-  The option ``-XExtendedDefaultRules`` is enabled, in order to apply
   special defaulting rules to expressions typed at the prompt (see
   :ref:`extended-default-rules`).

-  The Monomorphism Restriction is disabled (see :ref:`monomorphism`).

.. _ghci-dot-files:

The ``.ghci`` and ``.haskeline`` files
--------------------------------------

.. _dot-ghci-files:

The ``.ghci`` files
~~~~~~~~~~~~~~~~~~~

.. index::
   single: .ghci; file
   single: startup; files, GHCi

When it starts, unless the :ghc-flag:`-ignore-dot-ghci` flag is given, GHCi
reads and executes commands from the following files, in this order, if
they exist:

1. :file:`./.ghci`

2. :file:`{appdata}/ghc/ghci.conf`, where ⟨appdata⟩ depends on your system,
   but is usually something like
   :file:`C:/Documents and Settings/user/Application Data`

3. On Unix: :file:`$HOME/.ghc/ghci.conf`

4. :file:`$HOME/.ghci`

The :file:`ghci.conf` file is most useful for turning on favourite options
(e.g. ``:set +s``), and defining useful macros.

.. note::
    When setting language options in this file it is usually desirable to use
    :ghci-cmd:`:seti` rather than :ghci-cmd:`:set` (see :ref:`ghci-interactive-options`).

Placing a :file:`.ghci` file in a directory with a Haskell project is a
useful way to set certain project-wide options so you don't have to type
them every time you start GHCi: eg. if your project uses multi-parameter
type classes, scoped type variables, and CPP, and has source files in
three subdirectories A, B and C, you might put the following lines in
:file:`.ghci`:

.. code-block:: none

    :set -XMultiParamTypeClasses -XScopedTypeVariables -cpp
    :set -iA:B:C

(Note that strictly speaking the :ghc-flag:`-i` flag is a static one, but in
fact it works to set it using :ghci-cmd:`:set` like this. The changes won't take
effect until the next :ghci-cmd:`:load`, though.)

Once you have a library of GHCi macros, you may want to source them from
separate files, or you may want to source your ``.ghci`` file into your
running GHCi session while debugging it

.. code-block:: none

    :def source readFile

With this macro defined in your ``.ghci`` file, you can use
``:source file`` to read GHCi commands from ``file``. You can find (and
contribute!-) other suggestions for ``.ghci`` files on this Haskell wiki
page: `GHC/GHCi <http://haskell.org/haskellwiki/GHC/GHCi>`__

Additionally, any files specified with :ghc-flag:`-ghci-script` flags will be
read after the standard files, allowing the use of custom .ghci files.

Two command-line options control whether the startup files files are
read:

.. ghc-flag:: -ignore-dot-ghci

    Don't read either :file:`./.ghci` or the other startup files when
    starting up.

.. ghc-flag:: -ghci-script

    Read a specific file after the usual startup files. Maybe be
    specified repeatedly for multiple inputs.

When defining GHCi macros, there is some important behavior you should
be aware of when names may conflict with built-in commands, especially
regarding tab completion.

For example, consider if you had a macro named ``:time`` and in the
shell, typed ``:t 3`` — what should happen? The current algorithm we use
for completing commands is:

1. First, look up an exact match on the name from the defined macros.

2. Look for the exact match on the name in the built-in command list.

3. Do a prefix lookup on the list of built-in commands - if a built-in
   command matches, but a macro is defined with the same name as the
   built-in defined, pick the macro.

4. Do a prefix lookup on the list of built-in commands.

5. Do a prefix lookup on the list of defined macros.

Here are some examples:

1. You have a macro ``:time`` and enter ``:t 3``

   You get ``:type 3``

2. You have a macro ``:type`` and enter ``:t 3``

   You get ``:type 3`` with your defined macro, not the builtin.

3. You have a macro ``:time`` and a macro ``:type``, and enter ``:t 3``

   You get ``:type 3`` with your defined macro.

.. _dot-haskeline-file:

The ``.haskeline`` file
~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: .haskeline; file
   single: startup; files, GHCi

GHCi uses `Haskeline <https://hackage.haskell.org/package/haskeline>`__ under
the hood. You can configure it to, among other
things, prune duplicates from GHCi history. See:
`Haskeline user preferences <http://trac.haskell.org/haskeline/wiki/UserPrefs>`__.

.. _ghci-obj:

Compiling to object code inside GHCi
------------------------------------

By default, GHCi compiles Haskell source code into byte-code that is
interpreted by the runtime system. GHCi can also compile Haskell code to
object code: to turn on this feature, use the :ghc-flag:`-fobject-code` flag
either on the command line or with :ghci-cmd:`:set` (the option :ghc-flag:`-fbyte-code`
restores byte-code compilation again). Compiling to object code takes
longer, but typically the code will execute 10-20 times faster than
byte-code.

Compiling to object code inside GHCi is particularly useful if you are
developing a compiled application, because the :ghci-cmd:`:reload` command
typically runs much faster than restarting GHC with :ghc-flag:`--make` from the
command-line, because all the interface files are already cached in
memory.

There are disadvantages to compiling to object-code: you can't set
breakpoints in object-code modules, for example. Only the exports of an
object-code module will be visible in GHCi, rather than all top-level
bindings as in interpreted modules.

.. _external-interpreter:

Running the interpreter in a separate process
---------------------------------------------

Normally GHCi runs the interpreted code in the same process as GHC
itself, on top of the same RTS and sharing the same heap.  However, if
the flag :ghc-flag:`-fexternal-interpreter` is given, then GHC will spawn a
separate process for running interpreted code, and communicate with it
using messages over a pipe.

.. ghc-flag:: -fexternal-interpreter

    :since: 8.0.1

    Run interpreted code (for GHCi, Template Haskell, Quasi-quoting,
    or Annotations) in a separate process.  The interpreter will run
    in profiling mode if :ghc-flag:`-prof` is in effect, and in
    dynamically-linked mode if :ghc-flag:`-dynamic` is in effect.

    There are a couple of caveats that will hopefully be removed in
    the future: this option is currently not implemented on Windows
    (it is a no-op), and the external interpreter does not support the
    GHCi debugger, so breakpoints and single-stepping don't work with
    :ghc-flag:`-fexternal-interpreter`.

    See also the :ghc-flag:`-pgmi` (:ref:`replacing-phases`) and :ghc-flag:`-opti`
    (:ref:`forcing-options-through`) flags.

Why might we want to do this?  The main reason is that the RTS running
the interpreted code can be a different flavour (profiling or
dynamically-linked) from GHC itself.  So for example:

- We can use the profiler to collect stack traces when using GHCi (see
  :ref:`ghci-stack-traces`).

- When compiling Template Haskell code with :ghc-flag:`-prof` we don't need to
  compile the modules without :ghc-flag:`-prof` first (see :ref:`th-profiling`)
  because we can run the profiled object code in the interpreter.

This feature is experimental in GHC 8.0.x, but it may become the
default in future releases.

.. _ghci-faq:

FAQ and Things To Watch Out For
-------------------------------

The interpreter can't load modules with foreign export declarations!
    Unfortunately not. We haven't implemented it yet. Please compile any
    offending modules by hand before loading them into GHCi.

:ghc-flag:`-O` doesn't work with GHCi!

    .. index::
       single: optimization; and GHCi

    For technical reasons, the bytecode compiler doesn't interact well
    with one of the optimisation passes, so we have disabled
    optimisation when using the interpreter. This isn't a great loss:
    you'll get a much bigger win by compiling the bits of your code that
    need to go fast, rather than interpreting them with optimisation
    turned on.

Unboxed tuples don't work with GHCi
    That's right. You can always compile a module that uses unboxed
    tuples and load it into GHCi, however. (Incidentally the previous
    point, namely that :ghc-flag:`-O` is incompatible with GHCi, is because the
    bytecode compiler can't deal with unboxed tuples).

Concurrent threads don't carry on running when GHCi is waiting for input.
    This should work, as long as your GHCi was built with the
    :ghc-flag:`-threaded` switch, which is the default. Consult whoever supplied
    your GHCi installation.


After using ``getContents``, I can't use ``stdin``, until I do ``:load`` or ``:reload``
    This is the defined behaviour of ``getContents``: it puts the stdin
    Handle in a state known as semi-closed, wherein any further I/O
    operations on it are forbidden. Because I/O state is retained
    between computations, the semi-closed state persists until the next
    :ghci-cmd:`:load` or :ghci-cmd:`:reload` command.

    You can make ``stdin`` reset itself after every evaluation by giving
    GHCi the command ``:set +r``. This works because ``stdin`` is just a
    top-level expression that can be reverted to its unevaluated state
    in the same way as any other top-level expression (CAF).

I can't use :kbd:`Control-C` to interrupt computations in GHCi on Windows.
    See :ref:`ghci-windows`.

The default buffering mode is different in GHCi to GHC.
    In GHC, the stdout handle is line-buffered by default. However, in
    GHCi we turn off the buffering on stdout, because this is normally
    what you want in an interpreter: output appears as it is generated.

    If you want line-buffered behaviour, as in GHC, you can start your
    program thus: ::

        main = do { hSetBuffering stdout LineBuffering; ... }


.. [5]
   Note that packages only contain compiled code, so debugging a package
   requires finding its source and loading that directly.

.. [6]
   We originally provided bindings for all variables in scope, rather
   than just the free variables of the expression, but found that this
   affected performance considerably, hence the current restriction to
   just the free variables.
