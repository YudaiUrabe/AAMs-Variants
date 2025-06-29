## Static Analysis for Programs with Effect Handlers by Abstracting Abstract Machines

* `cek.ml`: Done
  * Different from the add/mul version, this implementation includes a function called `evaluate2`, which returns a single final state.

* `cesk.ml`: Done
  * Difference from the previous AM:
    _Adding a store to CEK machine and use it allocate variable bindings, thereby eliminating recursion from the environment in CEK machine_
    <!-- ＜対象言語＞ -->
    <!-- ＜抽象機械のシンタックス＞ -->
    * A new element (`store` ) was added to the configuration. Accordingly, the `storable` was introduced. This required changes to various parts of the implementation, including `inject`, `isFinal`, `isFinal2`, examples, and more.
    * A new `addr` type was introduced, also added `module AddrMap = Map.Make(Int)`.
    * The `env` now maps variables to addresses, not closures.
    * 
    * The `(///)` operator was added for `AddrMap` (used for store updates).
  
    <!-- ＜抽象機械のセマンティクス＞ -->
    * An `alloc` function was added to allocate fresh addresses (although it is not present in theory).
    * All transition functions now handle an extra element(store) in the machine state.
    * 
    * In the fourth case, both the environment and the store are changed.

    <!-- ＜テストや出力＞ -->
    * Since the recursive structure of `env` was removed, the `string_of_env` in the test was changed from `let rec` to just `let`.
    * The second argument of `binding_to_string` is now an address instead of a closure.
    * Added `string_of_store`.
  
* `cesk_star.ml`: Almost Done. Want to check the correctness automatically.
  * Difference from the previous AM(Changes were minimal.):
    <!-- ＜対象言語＞ -->
    <!-- ＜抽象機械のシンタックス＞ -->
    * A third element `addr` was added to both `Ar` and `Fn` in the `cont`. Accordingly, the continuation was changed from `kappa` to `a'` in the third case of the transition function.
    * Added `Cont` to the `storbale`.
    * 
    * The definition of `(///)` was modified so that it takes an `env` type rather than an arbitrary `map`.
    * Syntactic sugar `(///)` was added for updating the store.
    <!-- ＜抽象機械のセマンティクス＞ -->
    * In the second case of the transition function, the store and address were changed.
    * In the fourth case of the transition function, the continuation was changed.
    <!-- ＜テストや出力＞ -->
    * In the `let rec string_of_cont` of the auxiliary function for the tests, `string_of_cont k` was changed to `string_of_int a`.
    * Since `let string_of_store` uses `let rec string_of_cont`, the definitions were reordered so that `string_of_cont` comes ahead of `string_of_store`.

* `time_stapmed_CESK_star.ml`: Plan to change the outputs and check the correctness automatically.
  * Difference from the previous AM:
    * Added `time` as a new component of the machine state, which also affects `alloc` and `tick`.
    * Added `tick`function and modified all states (including tests) to include the fifth element for time. 
    * `alloc` is almost the same, but here it extracts the store from the input state.

* `abstract time-stapmed cesk*.ml`: temp
  * Difference from the previous AM:
    * Abstract only the `store`.

* (`krivine.ml`: To be tested
  * Difference from the CEK machine:
)
---

* `cek_handlers.ml`: To be tested
  * Difference from the CEK machine:
    <!-- ＜対象言語＞ -->
    * The target language has been enriched with more features.
    <!-- ＜抽象機械のシンタックス＞ -->
    * Tags, such as `Config2`, have been added to distinguish between different types of abstract machine states.
    * Function closures are embedded within the definition of values in the abstract machine, rather than defining them independently.
    *  


    <!-- ＜抽象機械のセマンティクス＞ -->
    * An interpretation function for values(`interpret_value`) is added, which differs from the standard CEK machine.
    * The `injection` and `eval` functions are defined over `comp` rather than `term`.
    * 
    <!-- ＜テストや出力＞ -->
    * In the current tests, intermediate steps are not displayed and only the first argument of the machine state is checked.



* `cesk_handlers.ml`: More refinement needed for the transition function

* `cesk*_handlers.ml`: temp

* `time-stapmed cesk*_handlers.ml`: temp

* `abstract time-stapmed cesk*_handlers.ml`: temp
---

 REFERENCES:
* David Van Horn and Matthew Might. 2010. Abstracting Abstract Machines. In Proceedings of the 15th ACM SIGPLAN
International Conference on Functional Programming (ICFP ’10). ACM, New York, NY, USA, 51-62.
* David Van Horn and Matthew Might. 2012. Systematic abstraction of abstract machines. In Journal of Functional Programming
22, 4-5 (2012), 705-746.
* Daniel Hillerström and Sam Lindley. Liberating effects with rows and handlers.
In TyDe@ICFP, pages 15–27. ACM, 2016.
* Daniel Hillerström. An Abstract Machine Semantics for Handlers, Mar 2017.
* Daniel Hillerström. Foundations for Programming and Implementing Effect Handlers. PhD dissertation, School of Informatics, The University of Edinburgh, 2021. 