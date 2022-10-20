\documentclass{article}

%include polycode.fmt
%options ghci -isrc

%format currXi0 = "\xi_0"
%format xiMinus0 = "\mu_{\xi_{-0}}"
%format condDistXiMinus0GivenXi0 = "\mu_{\xi_0}"
%format phi = "\phi"
%format rho = "\rho"
%format xi = "\xi"
%format xi' = "\xi^{\prime}"
%format alpha = "\alpha"
%format muu = "\mu"
%format mu0 = "\mu_0"
%format sigma = "\sigma"
%format sigma0 = "\sigma_0"
%format sigmaP = "\sigma_P"
%format truePosteriorMu = "\hat{\mu}"

\usepackage{amsmath}
\usepackage{mathrsfs}
\usepackage{amssymb}
\usepackage{natbib}

\usepackage[colorlinks, backref = page]{hyperref}
\hypersetup{
     colorlinks   = true,
     citecolor    = gray
}

\usepackage{svg}

\usepackage{graphicx}
\usepackage[left=0.25in, right=2.00in]{geometry}

\usepackage[textwidth=1.75in]{todonotes}

\setlength{\parskip}{\medskipamount}
\setlength{\parindent}{0pt}

\author{Dominic Steinitz}

\begin{document}

\title{A Haskell Perspective on a general perspective on the Metropolis–--Hastings kernel}

\maketitle

\listoftodos

\section{Introduction}

%if style == newcode
\begin{code}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports      #-}

{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (
    main
  , whatever
  , gradU'
  , algo1
  , truePosteriorMu
  , testStudentVarNorm
  , muu
  ) where

import Control.Monad.Bayes.Class hiding (posteriorPredictive, Histogram)
import Control.Monad.Bayes.Sampler.Strict

import Numeric.AD

import qualified Data.ByteString.Lazy as BL
import Data.Csv.Incremental (encodeRecord, encode)

import System.FilePath ()

import "monad-extras" Control.Monad.Extra

import Debug.Trace
\end{code}
%endif

In Bayesian statistics we have a prior distribution for the unknown
mean which we also take to be normal

$$
\mu \sim \mathcal{N}\left(\mu_0, \sigma_0^2\right)
$$

and then use a sample

$$
x \mid \mu \sim \mathcal{N}\left(\mu, \sigma^2\right)
$$

to produce a posterior distribution for it

$$
\mu \mid x \sim \mathcal{N}\left(\frac{\sigma_{0}^{2}}{\sigma^{2}+\sigma_{0}^{2}} x+\frac{\sigma^{2}}{\sigma^{2}+\sigma_{0}^{2}} \mu_{0},\left(\frac{1}{\sigma_{0}^{2}}+\frac{1}{\sigma^{2}}\right)^{-1}\right)
$$

If we continue to take samples then the posterior distribution becomes

$$
\mu \mid x_{1}, x_{2}, \cdots, x_{n} \sim \mathcal{N}\left(\frac{\sigma_{0}^{2}}{\frac{\sigma^{2}}{n}+\sigma_{0}^{2}} \bar{x}+\frac{\sigma^{2}}{\frac{\sigma^{2}}{n}+\sigma_{0}^{2}} \mu_{0},\left(\frac{1}{\sigma_{0}^{2}}+\frac{n}{\sigma^{2}}\right)^{-1}\right)
$$

Note that if we take $\sigma_0$ to be very large (we have little prior
information about the value of $\mu$) then

$$
\mu \mid x_{1}, x_{2}, \cdots, x_{n} \sim \mathcal{N}\left(\bar{x},\left(\frac{1}{\sigma_{0}^{2}}+\frac{n}{\sigma^{2}}\right)^{-1}\right)
$$

and if we take $n$ to be very large then

$$
\mu \mid x_{1}, x_{2}, \cdots, x_{n} \sim \mathcal{N}\left(\bar{x},\frac{\sigma}{\sqrt{n}}\right)
$$

which ties up with the classical estimate.

\begin{code}
muu, mu0, sigma0, sigma, sigmaP, z :: Floating a => a
muu = 3.0
mu0 = 0.0
sigma0 = 1.0
sigma = 0.5
sigmaP = 0.2
z = 4.0

truePosteriorMu :: Double
truePosteriorMu = z * sigma0^2 / (sigma^2 + sigma0^2) +
                  mu0 * sigma^2 / (sigma^2 + sigma0^2)
\end{code}

This gives \eval{truePosteriorMu}.

But suppose we don't know this result then we have from \cite{9117ec7157f141e28e5fe7bc575df48b}

$$
r\left(z, z^{\prime}\right)=\frac{\varpi\left(z^{\prime}\right) q\left(z^{\prime}, z\right)}{\varpi(z) q\left(z, z^{\prime}\right)}
$$

But where does this come from?

$$
\mu(\mathrm{d} \xi) \triangleq \pi\left(\mathrm{d} \xi_0\right) \mu_{\xi_0}\left(\mathrm{~d} \xi_{-0}\right)
$$

$$
\mu(\mathrm{d} (z, z^{\prime})) \triangleq \varpi\left(z\right)\mathrm{d} z ~ q_z\left(z^{\prime}\right)\mathrm{d} z^{\prime}
$$


Let $\mu$ be a finite measure on $(E, \mathscr{E}), \phi: E \rightarrow E$ an involution, let $\lambda \gg \mu$ be a $\sigma$-finite measure satisfying $\lambda \equiv \lambda^\phi$ and let $\rho=\mathrm{d} \mu / \mathrm{d} \lambda$. Then we can take $S=S\left(\mu, \mu^\phi\right)$ to be $S=\{\xi: \rho(\xi) \wedge \rho \circ \phi(\xi)>0\}$ and

$$
r(\xi)= \begin{cases}\frac{\rho \circ \phi}{\rho}(\xi) \frac{\mathrm{d} \lambda^\phi}{\mathrm{d} \lambda}(\xi) & \xi \in S, \\ 0 & \text { otherwise }\end{cases}
$$

So

$$
\rho(z, z^{\prime}) \triangleq \varpi\left(z\right) q_z\left(z^{\prime}\right)
$$

and with $\phi(z, z^{\prime}) = (z^{\prime}, z)$ we regain the familiar

$$
r\left(z, z^{\prime}\right)=\frac{\varpi\left({z^{\prime}}\right) q\left({z^{\prime}}, z\right)}{\varpi(z) q\left(z, z^{\prime}\right)}
$$

Here's {\bf Algorithm 1} from \cite{9117ec7157f141e28e5fe7bc575df48b}:

\begin{code}
algo1 :: Show a => Show b => (MonadDistribution m, Fractional t) =>
         (a, c) -> (a -> m b) -> ((a, b) -> (a, b)) -> (t -> Double) -> ((a, b) -> t) -> m (a, b)
algo1 (currXi0, _) condDistXiMinus0GivenXi0 phi a rho = do
  xiMinus0 <- condDistXiMinus0GivenXi0 currXi0
  let xi = (currXi0, xiMinus0)
  let alpha = a $ (rho . phi) xi / rho xi
  u <- random
  if u < alpha
    then return $ phi xi
    else return xi

bar d (u, p) = do
  u' <- random
  (v, q) <- d u'
  let alpha = min 1.0 (q / p)
  w <- random
  if w < alpha
    then return (v, q)
    else return (u, p)
\end{code}

Let's try running it on Student's T with 5 degrees of freedeom using
what I hope the textbook presentation of Metropolis---Hastings. The
probability density function (aka the Radon-Nikodym derivative wrt
Lebesgue measure) is

$$
f(t)=\frac{8}{3 \pi \sqrt{5}\left(1+\frac{t^2}{5}\right)^3}
$$

It's traditional to have $q_z(z^{\prime}) \sim {\mathcal{N}}(z, \sigma_p^2)$ for some given $\sigma_p$.

\begin{code}
testStudentVarNorm :: Floating a => a -> a
testStudentVarNorm t = studentVarPhiUnNorm t * 8 / (3 * pi * sqrt 5)

studentVarPhiUnNorm :: Floating a => a -> a
studentVarPhiUnNorm t =  1 / (1 + t^2/5)^3

qq :: Double -> Double -> Double
qq w w' = exp (-(w - w')^2 / (2 * sigmaP^2))

testRho :: (a -> Double) -> (a -> b -> Double) -> (a, b) -> Double
testRho varphi q (w, w') = varphi w * q w w'

testStudentMH :: MonadDistribution m =>
                  (Double, Double) -> m (Double, Double)
testStudentMH (xi0, _) = algo1 (xi0, undefined) (\xi00 -> normal xi00 sigmaP) (\(x, y) -> (y, x)) (min 1.0) (testRho studentVarPhiUnNorm qq)

testStudent2 :: (Eq a, Num a, MonadDistribution m) =>
                 a -> m [(Double, Double)]
testStudent2 n = unfoldM f (n, (0.0, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- testStudentMH s
                  return $ Just (s, (m - 1, x))

foo :: MonadDistribution m => (Double, b) -> m (Double, Double)
foo (xi0, _) = algo1 (xi0, undefined) (const (student4ICDF <$> random)) (\(x, y) -> (y, x)) (min 1.0) (student4 . fst)

student4 :: Floating a => a -> a
student4 t =  1 / (1 + t^2/4) ** (5 / 2)

student4ICDF :: Floating a => a -> a
student4ICDF p = signum (p - 1 / 2) * 2 * sqrt (q - 1)
  where
    q = cos ((1 / 3) * acos (sqrt alpha)) / sqrt alpha
    alpha = 4 * p * (1 - p)

testStudent3 :: (Eq a, Num a, MonadDistribution m) =>
                 a -> m [(Double, Double)]
testStudent3 n = unfoldM f (n, (0.2, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- foo s
                  return $ Just (s, (m - 1, x))
\end{code}

\section{Hamiltonian Monte Carlo}

We'd like to put HMC into the same general framework but at the
moment, I am having trouble squaring Example 14 in
\cite{9117ec7157f141e28e5fe7bc575df48b} with the algorithm given in
\cite{Neal2011MCMCUH} (and I haven't even looked in
\cite{https://doi.org/10.48550/arxiv.1410.5110}). Here's as far as I
got with Student's t-distribution of degree 5. There's something going
on with exponentiating the Hamiltonian which I don't understand yet
either.

\todo{I think I have squared some of this but need to write this down}

$$
f(t) = \frac{8}{3 \pi \sqrt{5}\left(1+\frac{t^2}{5}\right)^3}
$$

$$
g(t) = \frac{1}{\left(1+\frac{t^2}{5}\right)^3}
$$

$$
U(t) = -\log{f(t)} = 3 \log{(1 + \frac{r^2}{5})}
$$

\begin{code}
testGradU :: Fractional a => a -> a
testGradU r =  3 * (2 * r / 5) / (1 + (r^2) / 5)

leapfrog :: Fractional a => a -> Int -> (a -> a) -> (a, a) -> (a, a)
leapfrog epsilon l gradU (qPrev, p) = (q1, p3)
  where
  p' =  p - epsilon * gradU qPrev / 2
  f 0 (qOld, pOld) = r
    where
      qNew = qOld + epsilon * pOld
      pNew = pOld
      r = (qNew, pNew)
  f _ (qOld, pOld) = r
    where
      qNew = qOld + epsilon * pOld
      pNew = pOld - epsilon * gradU qNew
      r = (qNew, pNew)
  (q1, p1) = foldr f (qPrev, p') ([0 .. l - 1])
  p2 = p1 - epsilon * gradU q1 / 2
  -- Is this necessary?
  p3 = negate p2

expTestRhoHMC' :: Floating a => (b -> a) -> (b, a) -> a
expTestRhoHMC' u (t, r) = pU * pK
  where
    pU = recip $ u t
    pK = exp $ r^2 / 2
\end{code}

\begin{code}
eta :: Fractional a => a
eta = 0.3

bigL :: Int
bigL = 10

testPhi :: Fractional a => (a, a) -> (a, a)
testPhi = leapfrog eta bigL testGradU

reallyTestR'' :: (Eq a, MonadDistribution m, Num a) =>
                a -> m [(Double, Double)]
reallyTestR'' n = unfoldM f (n, (0.0, 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do a <- algo1 s (const $ normal 0.0 1.0) testPhi (min 1.0) (expTestRhoHMC' studentVarPhiUnNorm)
                  return $ Just (s, (m - 1, a))

writeToFile :: (Int -> SamplerIO [(Double, Double)]) -> FilePath -> Int -> IO ()
writeToFile t p n = do
  rs <- sampler (t n)
  let ss = zip ((map show [1 :: Int ..]) :: [String]) (map fst rs)
  BL.writeFile (p <> (show $ n) <> ".csv") $ encode $
    foldMap encodeRecord ss

main :: IO ()
main = do
  writeToFile reallyTestR'' "haskSampsHMC" 1000
  writeToFile testStudent2  "haskSampsMCMC" 1000
  writeToFile testStudent3  "haskSampsMB" 10000
\end{code}

\todo{It should make no difference what we return in the momentum
position; yet it does? Maybe not but at least investigate it.}

\begin{figure}[h]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/barChartGenericMCMC1.png}
    \caption{Sampling via MCMC and HMC}
    \label{fig:McmcAndHmc}
\end{figure}

\section{Posteriors}

For us, we want the posterior

$$
\varpi(\mu) = \frac{1}{Z}\exp{\frac{(x - \mu)^2}{2\sigma^2}} \exp{\frac{(\mu - \mu_0)^2}{2\sigma_0^2}}
$$

where $x, \mu_0, \sigma$ and $\sigma_0$ are all given.

\begin{code}
testVarphi :: Floating a => a -> a
testVarphi mu = exp (-(z - mu)^2 / (2 * sigma^2)) * exp (-(mu - mu0)^2 / (2 * sigma0^2))


testMH :: MonadDistribution m => (Double, Double) -> m (Double, Double)
testMH (xi0, _) = algo1 (xi0, undefined) (\xi00 -> normal xi00 sigmaP) (\(x, y) -> (y, x)) (min 1.0) (testRho testVarphi qq)

reallyTest2 :: (Eq a, Num a, MonadDistribution m) =>
               a -> m [(Double, Double)]
reallyTest2 n = unfoldM f (n, (1.0, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- testMH s
                  return $ Just (s, (m - 1, x))
\end{code}


\section{Whatever}

%if style == newcode
\begin{code}
bigU :: Floating a => a -> a
bigU = negate . log . studentVarPhiUnNorm


gradU' :: Floating a => a -> a
gradU' w = case grad (\[x] -> bigU x) $ [w] of
             [y] -> y
             _   -> error "Whatever"

whatever :: IO Double
whatever = (/ 2000) <$> sum <$> map fst <$> drop 2000 <$> sampler (reallyTest2 4000)
\end{code}
%endif

\section{Bibliography}

\bibliographystyle{plain}
\bibliography{references.bib}

\end{document}
