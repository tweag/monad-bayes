.. monad-bayes documentation master file, created by
   sphinx-quickstart on Fri Dec 17 18:38:02 2021.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Documentation for Monad-Bayes
=======================================

Monad-bayes is a library for doing probabilistic programming in Haskell. 
You can specify your model independent of the inference method as in other 
probabilistic programming languages. 
A unique feature is the modular construction of inference algorithms from simple parts. See this short paper for details: http://approximateinference.org/accepted/ScibiorGhahramani2016.pdf

In addition, Monad-Bayes is a normal Haskell library, not a separate language implemented using Haskell, 
so full interoperation with arbitrary Haskell code is totally straightforward.

.. The *user guide* shows how to write models in Monad-Bayes and perform inference.
.. The *developer guide* shows how the library works under the hood.


.. toctree::
   probprog
   usage
   :maxdepth: 2
   :caption: Contents: