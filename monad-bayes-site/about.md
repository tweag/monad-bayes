---
title: ""
---

Monad-Bayes is a library for **probabilistic programming** written in **Haskell**.

**Define distributions** [as programs](../Introduction.html)

**Perform inference** [with a variety of standard methods](/tutorials.html) 

**Integrate with Haskell code** [like this](/examples.html) because Monad-Bayes is just a library, not a separate language

<!-- ```haskell
example :: Distribution Histogram
example = fmap histogram (replicateM 1000 (normal 0 1))
``` -->

# Example


```haskell
model :: Distribution Double
model = do
     x <- bernoulli 0.5
     normal (if x then (-3) else 3) 1

image :: Distribution Plot
image = fmap (plot . histogram 200) (replicateM 100000 model)

sampler image
```

The program `model` is a mixture of Gaussians. Its type `Distribution Double` represents a distribution over reals. 
`image` is a program too: as its type shows, it is a distribution over plots. In particular, plots that arise from forming a 200 bin histogram out of 100000 independent identically distributed (iid) draws from `model`. 
To sample from `image`, we simply write `sampler image`, with the result shown below:


<img src="../images/plot.png" 
     width="450" 
     height="300" />

<!-- <html>
   <head>
   </head>

   <body>
      <div style = "position:relative; left:480px; top:2px; background-color:white;">
         
         
   </body>

</html>
<img src="../images/randomwalk.png" 
     width="400" 
     height="500" /> -->

<!-- (A sample from a distribution over diagrams, built by transforming a distribution over infinite lists, i.e. a random walk) -->
<!-- ](../randomwalk.png) -->
<!-- - why draw a sample and then make a plot when you can just define a distribution over plots instead (see todo). Sample from distributions over JSONs (see todo) or diagrams
- distributions over infinite (lazily evaluated) lists  are great for non-parametrics (see todo)
- build state space models as streams using `pipes`

inference methods are built modularly:

- write a particle filter that performs an MH step after every resampling step in one line (see todo) -->


