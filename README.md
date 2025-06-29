## Static Analysis for Programs with Effect Handlers by Abstracting Abstract Machines

* `cek.ml`: Done


* `cesk_star.ml`: Almost Done. Want to add one more test and check the correctness automatically.
  * Difference from the previous AM(Changes were minimal.):

    * In the `let rec string_of_cont` of the auxiliary function for the tests, `string_of_cont k` was changed to `string_of_int a`.
    * Since `let string_of_store` uses `let rec string_of_cont`, the definitions were reordered so that `string_of_cont` comes ahead of `string_of_store`.

* `time_stapmed_CESK_star.ml`: To be tested
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