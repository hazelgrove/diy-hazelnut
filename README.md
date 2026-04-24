# diy-hazelnut

A template for you to implement [Hazelnut](https://arxiv.org/pdf/1607.04180) yourself

## Setup

### Installing and Setting Up OCaml

You will need to have the OCaml toolchain installed. If you have not already it installed, I recommend the official [Get Up and Running With OCaml](https://ocaml.org/learn/tutorials/up_and_running.html) tutorial.

Once you do that, I recommend you setup an opam switch just for this project. You can think of a switch like an isolated environment to install opam packages. The packages you install in one switch won't interfere with the packages installed in another switch. This project has only been tested against OCaml 4.13.1, so we'll be setting up the switch with that version.

To create the switch:
```sh
opam switch create diy-hazelnut 5.2.0
```

To set this switch as the currently active one:
```sh
opam switch set diy-hazelnut
```

To list all the switches you have:
```sh
opam switch list
```

To learn more about switches:
```sh
man opam-switch
```

### Installing Dependencies

If you setup a switch for this project, make sure that it's active:
```sh
opam switch set diy-hazelnut
```

To install the dependencies to the currently active switch:
```sh
make deps
```

### Building, Running, etc.

All the build commands are managed by the `Makefile`. You're free to modify it to your own liking, but as it is provided, here's what the commands do:

To autoformat your code:
```sh
make fmt
```

To build the webapp:
```sh
make build
```

To autoformat and build:
```sh
make
```

To get the URL for the webapp:
```sh
make url
```

To erase the build:
```sh
make clean
```

To test your implementation:
```sh
make test
```

## Implementing Hazelnut

First of all, it's important that you understand what Hazelnut is and how it works. Read *[Hazelnut: A Bidirectionally Typed Structure Editor Calculus][hazelnut_paper]* if you haven't already. You don't have to understand everything right at this moment, but make sure that you have a good overview of how it all works.

You'll be using the Reason programming language, which essentially just OCaml with a more JavaScript-like syntax. It's the primary language used to implement [Hazel](https://github.com/hazelgrove/hazel). If you're already familiar with OCaml, but not Reason, [this website](https://reasonml.github.io/en/try) can be used to translate between OCaml and Reason. But if you'd really just prefer to use OCaml, you can convert code from OCaml to Reason using `refmt` as follows:

```refmt <file>.re -p ml > <file>.ml; rm <file>.re```

All the code you will write should go in [`hazelnut/hazelnut.re`](hazelnut/hazelnut.re). The starter code in there has been provided for your convenience, but you're free to modify it however you like. Modifying any other files (especially `hazelnut/hazelnut.rei`) isn't recommended, since it might cause unexpected problems.

A `typctx` is a map from variable names to types. You can insert/update values with `TypCtx.add`, and read values with `TypCtx.find`. See the [OCaml Map documentation](https://v2.ocaml.org/api/Map.Make.html) for more details.

### Guided Implementation Order

The starter code in `hazelnut.re` is organized to match the structure of the paper, with rule names in comments mapping each case to the formal system. The recommended order follows the dependency chain — each step only requires functions from earlier steps.

**Step 1: Type compatibility helpers** (Definitions 1-3)

Implement `consistent`, `inconsistent`, and `matched_arrow`. These are small, self-contained functions that the rest of the system depends on. The `helpers` test suite will verify them.

- `consistent(t1, t2)` — type consistency (Definition 1)
- `inconsistent(t1, t2)` — type inconsistency (Definition 2, just `!consistent`)
- `matched_arrow(t)` — extract arrow components, treating holes as arrows (Definition 3)

**Step 2: Cursor erasure** (Appendix A.2)

Implement `erase_typ` and `erase_exp`. These strip the cursor position from a zipper, recovering the underlying type or expression. They're needed by the action semantics and the webapp's theorem checks. The `erase_exp` and `helpers` test suites cover these.

**Step 3: Bidirectional type system** (Section 3.1)

Implement `syn` and `ana`, which are mutually recursive. `syn` returns `option(Htyp.t)` — the synthesized type — and `ana` returns `bool` — whether the expression checks against a given type. These use `consistent`, `inconsistent`, and `matched_arrow` from Step 1. The `syn` and `ana` test suites cover these.

Key things to watch for:
- `Lam` has no synthesis rule — it can only be checked analytically via `ana`.
- `ASubsume` is the fallthrough: if no analytic rule matches, try synthesizing and check consistency.

**Step 4: Action semantics** (Section 3.3)

This is the largest step. You'll need to implement several internal helpers first:

1. **`type_action`** — Type actions (move, delete, construct on type zippers). Start with the base cases (move into/out of arrows, delete, construct arrow/num), then add the zipper recursion rules.

2. **`move_exp`** — Expression movement. Handle each `Zexp` form's `Child(One)`/`Child(Two)`/`Parent` cases, then add zipper recursion (delegating to `type_action` for the `RAsc` case).

3. **`syn_action` and `ana_action`** — The main action functions (mutually recursive). Work through them in this order within the `switch`:
   - **Movement** — simplest, just delegates to `move_exp`
   - **Deletion** — replaces cursor target with a hole
   - **Construction** — the bulk of the rules (one case per `Shape` variant)
   - **Finishing** — unwraps non-empty holes
   - **Zipper rules** — recursive propagation through the tree

The `syn_action`, `ana_action`, `syn_zipper`, `type_action`, `properties`, `sample 1`, and `sample 2` test suites progressively exercise these.

> **Tip**: To test incrementally while your implementation is incomplete, use `raise(Unimplemented)` to fill the gaps. The webapp will warn you if it reaches an unimplemented part, but it won't crash.

## Test Suite

The test suite is organized to mirror the structure of the paper's Appendix A. Each test file targets a specific judgement or set of rules, and the test names reference the paper's rule labels (e.g. `SACONASC`, `ALAM`, `SAZIPPLUS1`).

| File | What it tests | Paper reference |
| :--- | :--- | :--- |
| `Test_helpers.re` | Type consistency, inconsistency, matched arrow, type erasure | Definitions 1-3, Appendix A.2.1 |
| `Test_erase_exp.re` | Expression cursor erasure | Appendix A.2.2 |
| `Test_syn.re` | Type synthesis (`syn`) | Rules 1a-1g |
| `Test_ana.re` | Type analysis (`ana`) | Rules 2a-2b |
| `Test_syn_action.re` | Synthetic action base cases | Rules 7a, 13a-13n, 15a, 16a |
| `Test_ana_action.re` | Analytic action base cases | Rules 5, 7b, 13b-13k, 15b, 16b, 18a |
| `Test_syn_zipper.re` | Synthetic action zipper rules | Rules 18b-18h |
| `Test_type_action.re` | Type actions (move, del, construct, zipper) | Rules 6a-6d, 12a-12b, 14, 17a-17b |
| `Test_properties.re` | Metatheorem verification (Sensibility, Movement Erasure Invariance) | Theorems 1-2 |
| `Test_qcheck.re` | Property-based random testing of metatheorems | Theorems 1-2 |
| `Test_sample1.re` | End-to-end: constructing the increment function (Figure 1) | Section 2.1 |
| `Test_sample2.re` | End-to-end: applying the increment function (Figure 2) | Section 2.2 |

**Design principle:** Tests deliberately use compound, non-value subexpressions (e.g. `Plus(Plus(Lit(2), Lit(3)), Plus(Lit(4), Lit(5)))` rather than `Plus(Lit(2), Lit(2))`) to catch the common student mistake of only handling leaf forms like `Lit` or `Var` in recursive positions.

The QCheck property-based tests (`Test_qcheck.re`) generate random well-typed expressions and verify the metatheorems hold on all of them. These tests pass vacuously on `Unimplemented` stubs, but once your implementation is complete, they serve as a powerful cross-check that your code satisfies the paper's formal guarantees.

## Using the Webapp

Once you've built the webapp with `make build`, you can open it in your browser. Run the command `make url` to get the URL.

The first thing you see will be the Hazelnut expression you're building (with cursor markers), the synthesized type, and the cursor-erased expression. Below that are the buttons that perform actions on the expression. If the button has an input field, you have to fill that in before pressing the button.

If anything unexpected happens, a warning will appear at the bottom. Here's what each warning means:

| Warning | Meaning |
| :--- | :--- |
| Invalid action | According to your implementation, that action cannot performed on the current expression. |
| Invalid input | The provided input isn't valid. |
| Unimplemented | You called upon an operation that you haven't implemented yet. |
| Theorem 1 violation | Your implementation has a bug: the erased result of the action doesn't synthesize the claimed output type (violates *Action Sensibility*). The message shows what was expected vs. what was found. |
| Theorem 2 violation | Your implementation has a bug: a movement action changed the cursor erasure or synthesized type (violates *Movement Erasure Invariance*). The message shows what changed. |


## Using the Maybe Monad

Have you ever written code that looks like this?

```reason
switch (a) {
| Some(b) =>
  switch (f(b)) {
  | Some(c) =>
    switch (g(c)) {
    | Some(d) => Some(h(d))
    | None => None
    }
  | None => None
  }
| None => None
};
```

That code is quite messy, even though it's just trying to express some simple logic: if the value is `Some(_)`, do something to it, but if it's `None`, just return `None`. Luckily there's a thing called the maybe monad. A category theorist could tell you all kinds of cool properties of monads, but for now, all you need to know is that they make it a lot easier to work with `option('a)` types.

The following is equivalent the block of code above, but much cleaner:

```reason
let* b = a;
let* c = f(b);
let+ d = g(c);
h(d);
```

Use `let*` if the expression below it evaluates to an `option('a)` type. Use `let+` if the expression below it evaluates to a `'a` type.

To use this monad syntax, uncomment `open Monad_lib.Monad;` at the top of the file.

## Extension: Sum Types (Section 4)

Once you've completed the core implementation, you can extend Hazelnut with binary sum types as described in Section 4 of the paper. This reinforces the methodology of how Hazelnut's metatheory guides language extensions.

The extension adds three new constructs:

| Construct | Syntax | Description |
| :--- | :--- | :--- |
| Sum type | `τ + τ` | Binary sum of two types |
| Injection | `inj[L](e)`, `inj[R](e)` | Inject a value into the left or right branch |
| Case analysis | `case(e, x.e₁, y.e₂)` | Eliminate a sum: if `e` is `inj[L](v)` bind `x=v` and evaluate `e₁`, etc. |

To implement this extension, you would need to:

1. **Extend the types.** Add `Sum(t, t)` to `Htyp.t`.

2. **Add a matched sum helper.** Like `matched_arrow`, define `matched_sum(τ)` that returns `Some((τ₁, τ₂))` for `Sum(τ₁, τ₂)` and holes, and `None` otherwise.

3. **Extend the expressions.** Add `InjL(e)`, `InjR(e)`, and `Case(e, string, e, string, e)` to `Hexp.t`.

4. **Extend the zippers.** Add cursor positions for each new subterm (e.g. `InjL(Zexp.t)`, `CaseScrut(Zexp.t, ...)`, `CaseL(Hexp.t, string, Zexp.t, ...)`, etc.).

5. **Add typing rules.** Synthesis for `case` and analysis for `inj[L]`/`inj[R]`, following the paper's Figure 13.

6. **Add action rules.** Construction and movement rules for the new forms.

The key insight from the paper is that each extension is *guided by the metatheory*: the types of the new judgement forms dictate which action rules are needed, and the Sensibility and Movement Erasure Invariance theorems tell you whether your rules are correct.

<!-- Link aliases -->

[hazelnut_paper]: https://arxiv.org/pdf/1607.04180
