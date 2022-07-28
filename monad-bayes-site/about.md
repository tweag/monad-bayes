---
title: ""
---

Monad-Bayes is a library for **probabilistic programming** written in **Haskell**.

**Define distributions** [as programs](../Introduction.html)

**Perform inference** [with a variety of standard methods, which you can extend](/tutorials.html) 

**Integrate with arbitrary Haskell code** [like this](/examples.html) because Monad-Bayes is just a library, not a separate language

<!-- ```haskell
example :: Distribution Histogram
example = fmap histogram (replicateM 1000 (normal 0 1))
``` -->



<html>
   <head>
   </head>

   <body>
      <div style = "position:relative; left:480px; top:2px; background-color:white;">
         
         
   </body>
</html>
<img src="../randomwalk.png" 
     width="400" 
     height="500" />

(A sample from a distribution over diagrams, built by transforming a distribution over infinite lists, i.e. a random walk)
<!-- ](../randomwalk.png) -->
<!-- - why draw a sample and then make a plot when you can just define a distribution over plots instead (see todo). Sample from distributions over JSONs (see todo) or diagrams
- distributions over infinite (lazily evaluated) lists  are great for non-parametrics (see todo)
- build state space models as streams using `pipes`

inference methods are built modularly:

- write a particle filter that performs an MH step after every resampling step in one line (see todo) -->


