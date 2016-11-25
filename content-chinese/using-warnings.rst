.. _options-sanity:

警告和可用性检查
----------------

.. index::
   single: 可用性检查的选项
   single: 警告

GHC 有许多标志可供你选择在编译过程中生成何种非致命错误信息的类型，
也就是我们所说的警告信息。默认情况下你会得到一组标准的警告信息，这些
警告普遍会用来提示你程序中的 bug。如下：

.. hlist::
    :columns: 3

    * :ghc-flag:`-Woverlapping-patterns`
    * :ghc-flag:`-Wwarnings-deprecations`
    * :ghc-flag:`-Wdeprecated-flags`
    * :ghc-flag:`-Wunrecognised-pragmas`
    * :ghc-flag:`-Wduplicate-constraints`
    * :ghc-flag:`-Wduplicate-exports`
    * :ghc-flag:`-Woverflowed-literals`
    * :ghc-flag:`-Wempty-enumerations`
    * :ghc-flag:`-Wmissing-fields`
    * :ghc-flag:`-Wmissing-methods`
    * :ghc-flag:`-Wwrong-do-bind`
    * :ghc-flag:`-Wunsupported-calling-conventions`
    * :ghc-flag:`-Wdodgy-foreign-imports`
    * :ghc-flag:`-Winline-rule-shadowing`
    * :ghc-flag:`-Wunsupported-llvm-version`
    * :ghc-flag:`-Wtabs`
    * :ghc-flag:`-Wunrecognised-warning-flags`

下面的这些标志是选择警告标准“包”的简单途径：

.. ghc-flag:: -W

    提供标准的警告以及

    .. hlist::
        :columns: 3

        * :ghc-flag:`-Wunused-binds`
        * :ghc-flag:`-Wunused-matches`
        * :ghc-flag:`-Wunused-foralls`
        * :ghc-flag:`-Wunused-imports`
        * :ghc-flag:`-Wincomplete-patterns`
        * :ghc-flag:`-Wdodgy-exports`
        * :ghc-flag:`-Wdodgy-imports`

.. ghc-flag:: -Wall

    打开所有警告信息的标志用以指明潜在存疑的代码。
    那些不是通过 :ghc-flag:`-Wall` 打开的警告有：

    .. hlist::
        :columns: 3

        * :ghc-flag:`-Wincomplete-uni-patterns`
        * :ghc-flag:`-Wincomplete-record-updates`
        * :ghc-flag:`-Wmonomorphism-restriction`
        * :ghc-flag:`-Wimplicit-prelude`
        * :ghc-flag:`-Wmissing-local-signatures`
        * :ghc-flag:`-Wmissing-exported-signatures`
        * :ghc-flag:`-Wmissing-import-lists`
        * :ghc-flag:`-Widentities`

.. ghc-flag:: -Wcompat

    开启了那些在将来会是默认开启但是目前在常规编译时仍然保持关闭的警告。这会令库
    的作者们乐于将他们的代码向后兼容以适配新的语言特性，而即便在此之前这些代码已
    经产生了警告。

    该标志目前开启了

    .. hlist::
        :columns: 3

        * :ghc-flag:`-Wmissing-monadfail-instances`
        * :ghc-flag:`-Wsemigroup`
        * :ghc-flag:`-Wnoncanonical-monoid-instances`

.. ghc-flag:: -Wno-compat

    关闭所有使用 :ghc-flag:`-Wcompat` 这个标志打开的警告。

.. ghc-flag:: -w

    关闭所有的警告，包括标准的和那些 :ghc-flag:`-Wall` 标志没有开启的。

.. ghc-flag:: -Werror

    将所有的警告都看作是一个致命错误。该标志在你进行批量编译过程中不想错过警告
    信息时会有用处。

.. ghc-flag:: -Wwarn

    警告仅仅是当作提示，而非错误。默认情况本是如此，但在想对 :ghc-flag:`-Werror` 这一
    标志进行取反时会有用。

当发出一个警告时，那个控制该警告的标志也同时会显示。

.. ghc-flag:: -fshow-warning-groups

    当显示一个警告是哪个标志控制的同时，还会显示包含该警告所在的每个
    警告组的标志。

    该标志默认是关闭的。

下面会列出警告的全套选项。想关闭任何一个警告的话，仅需在命令行给该选项加
一个相应的 ``-Wno-...`` 即可。如果想向前兼容8.0之前的 GHC 版本的话，可以
使用 ``-f(no-)warn-*`` 替代 ``-W(no-)*`` 来进行控制。

.. ghc-flag:: -Wunrecognised-warning-flags

    当编译器不能识别某个 ``-W...`` 标志时发出警告。

    该标志默认是开启的。

.. ghc-flag:: -Wtyped-holes

    是否让编译器报 typed holes 警告。除非 typed holes 错误被延迟到运行时，
    否则不会生效。见 :ref:`typed-holes` 和 :ref:`defer-type-errors`。

    该标志默认是开启的。

.. ghc-flag:: -Wtype-errors

    当一个类型错误被延迟到运行时的时候发出警告。见 :ref:`defer-type-errors`。

    该标志默认是开启的。

.. ghc-flag:: -fdefer-type-errors

    :涵盖了: :ghc-flag:`-fdefer-typed-holes`

    尽量延迟更多的类型错误至运行时。在编译阶段你会得到一个警告（而非错误）。
    在运行时阶段，如果你使用了一个和某个类型错误有依赖的值，你会得到一个运行时
    错误。但对于你代码中类型正确的那些部分都能运行无虞。见 :ref:`defer-type-errors`。

.. ghc-flag:: -fdefer-typed-holes

    延迟报 typed holes 错误（以下划线开头的命名错误，如“_”, “_foo”, “_bar”）至运行时。
    该标志会将由 :ref:`typed holes <typed-holes>` 产生的错误转化成警告。
    如同 :ghc-flag:`-fdefer-type-errors` 一样，使用一个依赖某个 typed hole 的值会产生
    一个运行时错误。见 :ref:`typed-holes` 与 :ref:`defer-type-errors`。

    该选项被 :ghc-flag:`-fdefer-type-errors` 所涵盖。另见 :ghc-flag:`-Wtyped-holes`。

.. ghc-flag:: -fdefer-out-of-scope-variables

    延迟变量超出作用域错误（有关命名非下划线开头的错误）至运行时。该选项会把变量超出
    作用域（variable-out-of-scope）错误转化成警告。使用一个依赖某个 typed hole 的值
    会产生一个运行时错误，同于 :ghc-flag:`-fdefer-type-errors` （该选项涵盖本选项）。
    见 :ref:`typed-holes` 与 :ref:`defer-type-errors`。

    该选项被 :ghc-flag:`-fdefer-type-errors` 所涵盖。 另见 :ghc-flag:`-Wdeferred-out-of-scope-variables`。

.. ghc-flag:: -Wpartial-type-signatures

    该选项决定编译器是否对部分类型签名（partial type signatures）中的 holes 报作警告。只有
    当开启了 :ghc-flag:`-XPartialTypeSignatures` 这个用来控制对于在类型中存在 holes 是否报错
    的选项时才会生效。见 :ref:`partial-type-signatures`。

    该警告默认是开启的。

.. ghc-flag:: -fhelpful-errors

    若在作用域内未发现某个命名或是包，则对可能是你想要的命名或是包做一个提示。

    该选项默认是开启的。

.. ghc-flag:: -Wunrecognised-pragmas

    当 GHC 无法识别某个编译选项时触发一个警告。除了 GHC 自己使用的那些编译选项外，
    GHC 还能识别一些熟知的其它工具所使用的编译选项，诸如 ``OPTIONS_HUGS`` 与 ``DERIVE``。

    该选项默认是开启的。

.. ghc-flag:: -Wmissed-specialisations
              -Wall-missed-specialisations

    当 GHC 不能实例化（specialise）一个重载的函数时会发出一个警告，这时通常是因为该函数需要
    一个 ``INLINABLE`` 的编译选项。使用带 "all" 的选项会报告所有的这样的情况，而不带 "all" 的
    形式仅对一个导入的函数在实例化过程中出现这样的问题时才发出警告。

    该不带 "all" 的选项多用于捕获这样一种情形：当一个导入的函数被标记为 ``INLINABLE`` （假定可
    实例化），而在它调用了其它不可实例化的函数后不能被实例化。

    注意这些警告在和 :ghc-flag:`-Werror` 共用时不会抛出错误。

    这两个选项默认都是关闭的。

.. ghc-flag:: -Wwarnings-deprecations

    .. index::
       pair: deprecations; warnings

    当使用了一个存在 ``WARNING`` 或 ``DEPRECATED pragma`` 问题的模块、函数或者类型
    时触发一个警告。有关编译选项的更多详情请见 :ref:`warning-deprecated-pragma`。

    该选项默认是开启的。

.. ghc-flag:: -Wamp

    .. index::
       single: AMP
       single: Applicative-Monad Proposal

    This option is deprecated.

    Caused a warning to be emitted when a definition was in conflict with
    the AMP (Applicative-Monad proosal).

.. ghc-flag:: -Wnoncanonical-monad-instances

    Warn if noncanonical ``Applicative`` or ``Monad`` instances
    declarations are detected.

    When this warning is enabled, the following conditions are verified:

    In ``Monad`` instances declarations warn if any of the following
    conditions does not hold:

     * If ``return`` is defined it must be canonical (i.e. ``return = pure``).
     * If ``(>>)`` is defined it must be canonical (i.e. ``(>>) = (*>)``).

    Moreover, in ``Applicative`` instance declarations:

     * Warn if ``pure`` is defined backwards (i.e. ``pure = return``).
     * Warn if ``(*>)`` is defined backwards (i.e. ``(*>) = (>>)``).

    该选项默认是关闭的。

.. ghc-flag:: -Wnoncanonical-monadfail-instances

    Warn if noncanonical ``Monad`` or ``MonadFail`` instances
    declarations are detected.

    When this warning is enabled, the following conditions are verified:

    In ``Monad`` instances declarations warn if any of the following
    conditions does not hold:

     * If ``fail`` is defined it must be canonical
       (i.e. ``fail = Control.Monad.Fail.fail``).

    Moreover, in ``MonadFail`` instance declarations:

     * Warn if ``fail`` is defined backwards
       (i.e. ``fail = Control.Monad.fail``).

    另见 :ghc-flag:`-Wmissing-monadfail-instances`.

    该选项默认是关闭的。

.. ghc-flag:: -Wnoncanonical-monoid-instances

    Warn if noncanonical ``Semigroup`` or ``Monoid`` instances
    declarations are detected.

    When this warning is enabled, the following conditions are verified:

    In ``Monoid`` instances declarations warn if any of the following
    conditions does not hold:

     * If ``mappend`` is defined it must be canonical
       (i.e. ``mappend = (Data.Semigroup.<>)``).

    Moreover, in ``Semigroup`` instance declarations:

     * Warn if ``(<>)`` is defined backwards (i.e. ``(<>) = mappend``).

    该警告默认是关闭的。 However, it is part of the
    :ghc-flag:`-Wcompat` option group.

.. ghc-flag:: -Wmissing-monadfail-instances

    .. index::
       single: MFP
       single: MonadFail Proposal

    Warn when a failable pattern is used in a do-block that does not have a
    ``MonadFail`` instance.

    另见 :ghc-flag:`-Wnoncanonical-monadfail-instances`.

    Being part of the :ghc-flag:`-Wcompat` option group, this warning is off by
    default, but will be switched on in a future GHC release, as part of
    the `MonadFail Proposal (MFP)
    <https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail>`__.

.. ghc-flag:: -Wsemigroup

    .. index::
       single: semigroup; warning

    Warn when definitions are in conflict with the future inclusion of
    ``Semigroup`` into the standard typeclasses.

     1. Instances of ``Monoid`` should also be instances of ``Semigroup``
     2. The ``Semigroup`` operator ``(<>)`` will be in ``Prelude``, which
        clashes with custom local definitions of such an operator

    Being part of the :ghc-flag:`-Wcompat` option group, this warning is off by
    default, but will be switched on in a future GHC release.

.. ghc-flag:: -Wdeprecated-flags

    .. index::
       single: deprecated flags

    Causes a warning to be emitted when a deprecated command-line flag
    is used.

    该选项默认是开启的.

.. ghc-flag:: -Wunsupported-calling-conventions

    Causes a warning to be emitted for foreign declarations that use
    unsupported calling conventions. In particular, if the ``stdcall``
    calling convention is used on an architecture other than i386 then
    it will be treated as ``ccall``.

.. ghc-flag:: -Wdodgy-foreign-imports

    Causes a warning to be emitted for foreign imports of the following
    form: ::

        foreign import "f" f :: FunPtr t

    on the grounds that it probably should be ::

        foreign import "&f" f :: FunPtr t

    The first form declares that \`f\` is a (pure) C function that takes
    no arguments and returns a pointer to a C function with type \`t\`,
    whereas the second form declares that \`f\` itself is a C function
    with type \`t\`. The first declaration is usually a mistake, and one
    that is hard to debug because it results in a crash, hence this
    warning.

.. ghc-flag:: -Wdodgy-exports

    Causes a warning to be emitted when a datatype ``T`` is exported
    with all constructors, i.e. ``T(..)``, but is it just a type
    synonym.

    Also causes a warning to be emitted when a module is re-exported,
    but that module exports nothing.

.. ghc-flag:: -Wdodgy-imports

    Causes a warning to be emitted in the following cases:

    -  When a datatype ``T`` is imported with all constructors, i.e.
       ``T(..)``, but has been exported abstractly, i.e. ``T``.

    -  When an ``import`` statement hides an entity that is not
       exported.

.. ghc-flag:: -Woverflowed-literals

    Causes a warning to be emitted if a literal will overflow, e.g.
    ``300 :: Word8``.

.. ghc-flag:: -Wempty-enumerations

    Causes a warning to be emitted if an enumeration is empty, e.g.
    ``[5 .. 3]``.

.. ghc-flag:: -Wduplicate-constraints

    .. index::
       single: duplicate constraints, warning

    Have the compiler warn about duplicate constraints in a type
    signature. For example ::

        f :: (Eq a, Show a, Eq a) => a -> a

    The warning will indicate the duplicated ``Eq a`` constraint.

    This option is now deprecated in favour of
    :ghc-flag:`-Wredundant-constraints`.

.. ghc-flag:: -Wredundant-constraints

    :since: 8.0

    .. index::
       single: redundant constraints, warning

    Have the compiler warn about redundant constraints in a type
    signature. In particular:

    -  A redundant constraint within the type signature itself: ::

            f :: (Eq a, Ord a) => a -> a

       The warning will indicate the redundant ``Eq a`` constraint: it
       is subsumed by the ``Ord a`` constraint.

    -  A constraint in the type signature is not used in the code it
       covers: ::

            f :: Eq a => a -> a -> Bool
            f x y = True

       The warning will indicate the redundant ``Eq a`` constraint: : it
       is not used by the definition of ``f``.)

    Similar warnings are given for a redundant constraint in an instance
    declaration.

    该选项默认是开启的. As usual you can suppress it on a
    per-module basis with :ghc-flag:`-Wno-redundant-constraints`.
    Occasionally you may specifically want a function to have a more
    constrained signature than necessary, perhaps to leave yourself
    wiggle-room for changing the implementation without changing the
    API. In that case, you can suppress the warning on a per-function
    basis, using a call in a dead binding. For example: ::

        f :: Eq a => a -> a -> Bool
        f x y = True
        where
            _ = x == x  -- Suppress the redundant-constraint warning for (Eq a)

    Here the call to ``(==)`` makes GHC think that the ``(Eq a)``
    constraint is needed, so no warning is issued.

.. ghc-flag:: -Wduplicate-exports

    .. index::
       single: duplicate exports, warning
       single: export lists, duplicates

    Have the compiler warn about duplicate entries in export lists. This
    is useful information if you maintain large export lists, and want
    to avoid the continued export of a definition after you've deleted
    (one) mention of it in the export list.

    该选项默认是开启的.

.. ghc-flag:: -Whi-shadowing

    .. index::
       single: shadowing; interface files

    Causes the compiler to emit a warning when a module or interface
    file in the current directory is shadowing one with the same module
    name in a library or other directory.

.. ghc-flag:: -Widentities

    Causes the compiler to emit a warning when a Prelude numeric
    conversion converts a type ``T`` to the same type ``T``; such calls are
    probably no-ops and can be omitted. The functions checked for are:
    ``toInteger``, ``toRational``, ``fromIntegral``, and ``realToFrac``.

.. ghc-flag:: -Wimplicit-prelude

    .. index::
       single: implicit prelude, warning

    Have the compiler warn if the Prelude is implicitly imported. This
    happens unless either the Prelude module is explicitly imported with
    an ``import ... Prelude ...`` line, or this implicit import is
    disabled (either by :ghc-flag:`-XNoImplicitPrelude` or a
    ``LANGUAGE NoImplicitPrelude`` pragma).

    Note that no warning is given for syntax that implicitly refers to
    the Prelude, even if :ghc-flag:`-XNoImplicitPrelude` would change whether it
    refers to the Prelude. For example, no warning is given when ``368``
    means ``Prelude.fromInteger (368::Prelude.Integer)`` (where
    ``Prelude`` refers to the actual Prelude module, regardless of the
    imports of the module being compiled).

    该警告默认是关闭的。

.. ghc-flag:: -Wincomplete-patterns
              -Wincomplete-uni-patterns

    .. index::
       single: incomplete patterns, warning
       single: patterns, incomplete

    The option :ghc-flag:`-Wincomplete-patterns` warns about places where a
    pattern-match might fail at runtime. The function ``g`` below will
    fail when applied to non-empty lists, so the compiler will emit a
    warning about this when :ghc-flag:`-Wincomplete-patterns` is enabled. ::

        g [] = 2

    This option isn't enabled by default because it can be a bit noisy,
    and it doesn't always indicate a bug in the program. However, it's
    generally considered good practice to cover all the cases in your
    functions, and it is switched on by :ghc-flag:`-W`.

    The flag :ghc-flag:`-Wincomplete-uni-patterns` is similar, except that
    it applies only to lambda-expressions and pattern bindings,
    constructs that only allow a single pattern: ::

        h = \[] -> 2
        Just k = f y

.. ghc-flag:: -fmax-pmcheck-iterations=<N>

    :default: 2000000

    Sets how many iterations of the pattern-match checker will perform before
    giving up. This limit is to catch cases where pattern-match checking might
    be excessively costly (due to the exponential complexity of coverage
    checking in the general case). It typically shouldn't be necessary to set
    this unless GHC informs you that it has exceeded the pattern match checker's
    iteration limit (in which case you may want to consider refactoring your
    pattern match, for the sake of future readers of your code.

.. ghc-flag:: -Wincomplete-record-updates

    .. index::
       single: incomplete record updates, warning
       single: record updates, incomplete

    The function ``f`` below will fail when applied to ``Bar``, so the
    compiler will emit a warning about this when
    :ghc-flag:`-Wincomplete-record-updates` is enabled. ::

        data Foo = Foo { x :: Int }
                 | Bar

        f :: Foo -> Foo
        f foo = foo { x = 6 }

    This option isn't enabled by default because it can be very noisy,
    and it often doesn't indicate a bug in the program.

.. ghc-flag:: -Wmissing-fields

    .. index::
       single: missing fields, warning
       single: fields, missing

    该选项默认是开启的, and warns you whenever the
    construction of a labelled field constructor isn't complete, missing
    initialisers for one or more fields. While not an error (the missing
    fields are initialised with bottoms), it is often an indication of a
    programmer error.

.. ghc-flag:: -Wmissing-import-lists

    .. index::
       single: missing import lists, warning
       single: import lists, missing

    This flag warns if you use an unqualified ``import`` declaration
    that does not explicitly list the entities brought into scope. For
    example ::

        module M where
          import X( f )
          import Y
          import qualified Z
          p x = f x x

    The :ghc-flag:`-Wmissing-import-lists` flag will warn about the import of
    ``Y`` but not ``X`` If module ``Y`` is later changed to export (say) ``f``,
    then the reference to ``f`` in ``M`` will become ambiguous. No warning is
    produced for the import of ``Z`` because extending ``Z``\'s exports would be
    unlikely to produce ambiguity in ``M``.

.. ghc-flag:: -Wmissing-methods

    .. index::
       single: missing methods, warning
       single: methods, missing

    该选项默认是开启的, and warns you whenever an instance
    declaration is missing one or more methods, and the corresponding
    class declaration has no default declaration for them.

    The warning is suppressed if the method name begins with an
    underscore. Here's an example where this is useful: ::

        class C a where
            _simpleFn :: a -> String
            complexFn :: a -> a -> String
            complexFn x y = ... _simpleFn ...

    The idea is that: (a) users of the class will only call
    ``complexFn``; never ``_simpleFn``; and (b) instance declarations
    can define either ``complexFn`` or ``_simpleFn``.

    The ``MINIMAL`` pragma can be used to change which combination of
    methods will be required for instances of a particular class. See
    :ref:`minimal-pragma`.

.. ghc-flag:: -Wmissing-signatures

    .. index::
       single: type signatures, missing

    If you would like GHC to check that every top-level function/value
    has a type signature, use the :ghc-flag:`-Wmissing-signatures` option.
    As part of the warning GHC also reports the inferred type. The
    option is off by default.

.. ghc-flag:: -Wmissing-exported-sigs

    .. index::
       single: type signatures, missing

    This option is now deprecated in favour of
    :ghc-flag:`-Wmissing-exported-signatures`.

.. ghc-flag:: -Wmissing-exported-signatures

    .. index::
       single: type signatures, missing

    If you would like GHC to check that every exported top-level
    function/value has a type signature, but not check unexported
    values, use the :ghc-flag:`-Wmissing-exported-signatures`
    option. This option takes precedence over
    :ghc-flag:`-Wmissing-signatures`. As part of the warning GHC also
    reports the inferred type. The option is off by default.

.. ghc-flag:: -Wmissing-local-sigs

    .. index::
       single: type signatures, missing

    This option is now deprecated in favour of
    :ghc-flag:`-Wmissing-local-signatures`.

.. ghc-flag:: -Wmissing-local-signatures

    .. index::
       single: type signatures, missing

    If you use the :ghc-flag:`-Wmissing-local-signatures` flag GHC
    will warn you about any polymorphic local bindings. As part of the
    warning GHC also reports the inferred type. The option is off by
    default.

.. ghc-flag:: -Wmissing-pattern-synonym-signatures

    .. index::
         single: type signatures, missing, pattern synonyms

    If you would like GHC to check that every pattern synonym has a
    type signature, use the
    :ghc-flag:`-Wmissing-pattern-synonym-signatures` option. If this
    option is used in conjunction with
    :ghc-flag:`-Wmissing-exported-signatures` then only exported pattern
    synonyms must have a type signature. GHC also reports the inferred
    type. 该选项默认是关闭的。

.. ghc-flag:: -Wname-shadowing

    .. index::
       single: shadowing, warning

    This option causes a warning to be emitted whenever an inner-scope
    value has the same name as an outer-scope value, i.e. the inner
    value shadows the outer one. This can catch typographical errors
    that turn into hard-to-find bugs, e.g., in the inadvertent capture
    of what would be a recursive call in
    ``f = ... let f = id in ... f ...``.

    The warning is suppressed for names beginning with an underscore.
    For example ::

        f x = do { _ignore <- this; _ignore <- that; return (the other) }

.. ghc-flag:: -Worphans

    .. index::
       single: orphan instances, warning
       single: orphan rules, warning

    These flags cause a warning to be emitted whenever the module
    contains an "orphan" instance declaration or rewrite rule. An
    instance declaration is an orphan if it appears in a module in which
    neither the class nor the type being instanced are declared in the
    same module. A rule is an orphan if it is a rule for a function
    declared in another module. A module containing any orphans is
    called an orphan module.

    The trouble with orphans is that GHC must pro-actively read the
    interface files for all orphan modules, just in case their instances
    or rules play a role, whether or not the module's interface would
    otherwise be of any use. See :ref:`orphan-modules` for details.

    The flag :ghc-flag:`-Worphans` warns about user-written orphan rules or
    instances.

.. ghc-flag:: -Woverlapping-patterns

    .. index::
       single: overlapping patterns, warning
       single: patterns, overlapping

    By default, the compiler will warn you if a set of patterns are
    overlapping, e.g., ::

        f :: String -> Int
        f []     = 0
        f (_:xs) = 1
        f "2"    = 2

    where the last pattern match in ``f`` won't ever be reached, as the
    second pattern overlaps it. More often than not, redundant patterns
    is a programmer mistake/error, so this option is enabled by default.

.. ghc-flag:: -Wsimplifiable-class-constraints

    :since: 8.2

    .. index::
       single: simplifiable class constraints, warning

    Warn about class constraints in a type signature that can be simplified
    using a top-level instance declaration.  For example: ::

       f :: Eq [a] => a -> a

    Here the ``Eq [a]`` in the signature overlaps with the top-level
    instance for ``Eq [a]``.  GHC goes to some efforts to use the former,
    but if it should use the latter, it would then have an
    insoluble ``Eq a`` constraint.  Best avoided by instead writing: ::

       f :: Eq a => a -> a

    该选项默认是开启的. As usual you can suppress it on a
    per-module basis with :ghc-flag:`-Wno-simplifiable-class-constraints`.

.. ghc-flag:: -Wtabs

    .. index::
       single: tabs, warning

    Have the compiler warn if there are tabs in your source file.

.. ghc-flag:: -Wtype-defaults

    .. index::
       single: defaulting mechanism, warning

    Have the compiler warn/inform you where in your source the Haskell
    defaulting mechanism for numeric types kicks in. This is useful
    information when converting code from a context that assumed one
    default into one with another, e.g., the ‘default default’ for
    Haskell 1.4 caused the otherwise unconstrained value ``1`` to be
    given the type ``Int``, whereas Haskell 98 and later defaults it to
    ``Integer``. This may lead to differences in performance and
    behaviour, hence the usefulness of being non-silent about this.

    该警告默认是关闭的。

.. ghc-flag:: -Wmonomorphism-restriction

    .. index::
       single: monomorphism restriction, warning

    Have the compiler warn/inform you where in your source the Haskell
    Monomorphism Restriction is applied. If applied silently the MR can
    give rise to unexpected behaviour, so it can be helpful to have an
    explicit warning that it is being applied.

    该警告默认是关闭的。

.. ghc-flag:: -Wunsupported-llvm-version

    Warn when using :ghc-flag:`-fllvm` with an unsupported version of LLVM.

.. ghc-flag:: -Wunticked-promoted-constructors

    .. index::
       single: promoted constructor, warning

    Warn if a promoted data constructor is used without a tick preceding
    its name.

    For example: ::

        data Nat = Succ Nat | Zero

        data Vec n s where
          Nil  :: Vec Zero a
          Cons :: a -> Vec n a -> Vec (Succ n) a

    Will raise two warnings because ``Zero`` and ``Succ`` are not
    written as ``'Zero`` and ``'Succ``.

    This warning is is enabled by default in :ghc-flag:`-Wall` mode.

.. ghc-flag:: -Wunused-binds

    .. index::
       single: unused binds, warning
       single: binds, unused

    Report any function definitions (and local bindings) which are
    unused. An alias for

    -  :ghc-flag:`-Wunused-top-binds`
    -  :ghc-flag:`-Wunused-local-binds`
    -  :ghc-flag:`-Wunused-pattern-binds`

.. ghc-flag:: -Wunused-top-binds

    .. index::
       single: unused binds, warning
       single: binds, unused

    Report any function definitions which are unused.

    More precisely, warn if a binding brings into scope a variable that
    is not used, except if the variable's name starts with an
    underscore. The "starts-with-underscore" condition provides a way to
    selectively disable the warning.

    A variable is regarded as "used" if

    -  It is exported, or

    -  It appears in the right hand side of a binding that binds at
       least one used variable that is used

    For example: ::

        module A (f) where
        f = let (p,q) = rhs1 in t p  -- No warning: q is unused, but is locally bound
        t = rhs3                     -- No warning: f is used, and hence so is t
        g = h x                      -- Warning: g unused
        h = rhs2                     -- Warning: h is only used in the
                                     -- right-hand side of another unused binding
        _w = True                    -- No warning: _w starts with an underscore

.. ghc-flag:: -Wunused-local-binds

    .. index::
       single: unused binds, warning
       single: binds, unused

    Report any local definitions which are unused. For example: ::

        module A (f) where
        f = let (p,q) = rhs1 in t p  -- Warning: q is unused
        g = h x                      -- No warning: g is unused, but is a top-level binding

.. ghc-flag:: -Wunused-pattern-binds

    .. index::
       single: unused binds, warning
       single: binds, unused

    Warn if a pattern binding binds no variables at all, unless it is a
    lone, possibly-banged, wild-card pattern. For example: ::

        Just _ = rhs3    -- Warning: unused pattern binding
        (_, _) = rhs4    -- Warning: unused pattern binding
        _  = rhs3        -- No warning: lone wild-card pattern
        !_ = rhs4        -- No warning: banged wild-card pattern; behaves like seq

    The motivation for allowing lone wild-card patterns is they are not
    very different from ``_v = rhs3``, which elicits no warning; and
    they can be useful to add a type constraint, e.g. ``_ = x::Int``. A
    lone banged wild-card pattern is useful as an alternative (to
    ``seq``) way to force evaluation.

.. ghc-flag:: -Wunused-imports

    .. index::
       single: unused imports, warning
       single: imports, unused

    Report any modules that are explicitly imported but never used.
    However, the form ``import M()`` is never reported as an unused
    import, because it is a useful idiom for importing instance
    declarations, which are anonymous in Haskell.

.. ghc-flag:: -Wunused-matches

    .. index::
       single: unused matches, warning
       single: matches, unused

    Report all unused variables which arise from term-level pattern matches,
    including patterns consisting of a single variable. For instance
    ``f x y = []`` would report ``x`` and ``y`` as unused. The warning
    is suppressed if the variable name begins with an underscore, thus: ::

        f _x = True

    Note that :ghc-flag:`-Wunused-matches` does not warn about variables which
    arise from type-level patterns, as found in type family and data family
    instances. This must be enabled separately through the
    :ghc-flag:`-Wunused-type-patterns` flag.

.. ghc-flag:: -Wunused-do-bind

    .. index::
       single: unused do binding, warning
       single: do binding, unused

    Report expressions occurring in ``do`` and ``mdo`` blocks that
    appear to silently throw information away. For instance
    ``do { mapM popInt xs ; return 10 }`` would report the first
    statement in the ``do`` block as suspicious, as it has the type
    ``StackM [Int]`` and not ``StackM ()``, but that ``[Int]`` value is
    not bound to anything. The warning is suppressed by explicitly
    mentioning in the source code that your program is throwing
    something away: ::

        do { _ <- mapM popInt xs ; return 10 }

    Of course, in this particular situation you can do even better: ::

        do { mapM_ popInt xs ; return 10 }

.. ghc-flag:: -Wunused-type-patterns

    .. index::
       single: unused type patterns, warning
       single: type patterns, unused

    Report all unused type variables which arise from patterns in type family
    and data family instances. For instance: ::

        type instance F x y = []

    would report ``x`` and ``y`` as unused. The warning is suppressed if the
    type variable name begins with an underscore, like so: ::

        type instance F _x _y = []

    Unlike :ghc-flag:`-Wunused-matches`, :ghc-flag:`-Wunused-type-variables` is
    not implied by :ghc-flag:`-Wall`. The rationale for this decision is that
    unlike term-level pattern names, type names are often chosen expressly for
    documentation purposes, so using underscores in type names can make the
    documentation harder to read.

.. ghc-flag:: -Wunused-foralls

    .. index::
       single: unused foralls, warning
       single: foralls, unused

    Report all unused type variables which arise from explicit, user-written
    ``forall`` statements. For instance: ::

        g :: forall a b c. (b -> b)

    would report ``a`` and ``c`` as unused.

.. ghc-flag:: -Wwrong-do-bind

    .. index::
       single: apparently erroneous do binding, warning
       single: do binding, apparently erroneous

    Report expressions occurring in ``do`` and ``mdo`` blocks that
    appear to lack a binding. For instance
    ``do { return (popInt 10) ; return 10 }`` would report the first
    statement in the ``do`` block as suspicious, as it has the type
    ``StackM (StackM Int)`` (which consists of two nested applications
    of the same monad constructor), but which is not then "unpacked" by
    binding the result. The warning is suppressed by explicitly
    mentioning in the source code that your program is throwing
    something away: ::

        do { _ <- return (popInt 10) ; return 10 }

    For almost all sensible programs this will indicate a bug, and you
    probably intended to write: ::

        do { popInt 10 ; return 10 }

.. ghc-flag:: -Winline-rule-shadowing

    Warn if a rewrite RULE might fail to fire because the function might
    be inlined before the rule has a chance to fire. See
    :ref:`rules-inline`.

If you're feeling really paranoid, the :ghc-flag:`-dcore-lint` option is a good choice.
It turns on heavyweight intra-pass sanity-checking within GHC. (It checks GHC's
sanity, not yours.)

如果你还是心存怀疑，那么使用 :ghc-flag:`-dcore-lint` 选项是个不错的选择，它会
打开 GHC 内部 pass 之间最严格的可用性检查。（它检查的是GHC是否粗心大意，而不是你）

