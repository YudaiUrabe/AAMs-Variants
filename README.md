## Static Analysis for Programs with Effect Handlers by Abstracting Abstract Machines

* `cek.ml`: Done


* `cesk.ml`: Done
  * Difference from the previous AM:


* `cesk_star.ml`: Almost Done. Want to add one more test and check the correctness automatically.



* `time_stapmed_CESK_star.ml`: To be tested
  * Difference from the previous AM:


* `abstract time-stapmed cesk*.ml`: temp
  * Difference from the previous AM:


* (`krivine.ml`: To be tested
  * Difference from the CEK machine:
)
---

* `cek_handlers.ml`: To be tested
  * Difference from the CEK machine:

  - The target language has been enriched with more features.


  - Tags, such as 'Config2', have been added to distinguish between different types of abstract machine states.
  - Function closures are embedded within the definition of values in the abstract machine, rather than defining them independently.
  - 

  - An interpretation function for values('interpret_value') is added, which differs from the standard CEK machine.
  - The 'injection' and 'eval' functions are defined over 'comp' rather than 'term'.
  - 

  - In the current tests, intermediate steps are not displayed and only the first argument of the machine state is checked.



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