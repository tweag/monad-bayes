\documentclass{article}

%include polycode.fmt
%options ghci -isrc

%format currXi0 = "\xi_0"
%format xiMinus0 = "\mu_{\xi_{-0}}"
%format condDistXiMinus0GivenXi0 = "\mu_{\xi_0}"
%format phi = "\phi"
%format rho = "\rho"
%format testRho = "\tilde{\rho}"
%format xi = "\xi"
%format xi00 = "\zeta"
%format xi0 = "\xi_0"
%format xi' = "\xi^{\prime}"
%format alpha = "\alpha"
%format mu0 = "\mu_0"
%format mu = "\mu"
%format sigma = "\sigma"
%format sigma0 = "\sigma_0"
%format sigmaP = "\sigma_P"
%format truePosteriorMu = "\hat{\mu}"
%format truePosteriorSigma = "\hat{\sigma}"
%format qq = "\mathfrak{Q}"
%format varphi = "\varphi"
%format testVarphi = "\tilde{\varphi}"

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
  , gradU'
  , algo1
  , truePosteriorMu
  , truePosteriorSigma
  , student5
  ) where

import Control.Monad.Bayes.Class hiding (posteriorPredictive, Histogram)
import Control.Monad.Bayes.Sampler.Strict

import Numeric.AD

import qualified Data.ByteString.Lazy as BL
import Data.Csv.Incremental (encodeRecord, encode)

import System.FilePath ()

import Statistics.Distribution.StudentT -- (studentT, StudentT)
import Statistics.Distribution.Normal -- (normalDistr, NormalDistribution)
import Statistics.Distribution -- (quantile)

import "monad-extras" Control.Monad.Extra
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
mu0, sigma0, sigma, sigmaP, z :: Floating a => a
mu0 = 0.0
sigma0 = 1.0
sigma = 1.0
sigmaP = 0.2
z = 4.0

truePosteriorMu :: Double
truePosteriorMu = z * sigma0^2 / (sigma^2 + sigma0^2) +
                  mu0 * sigma^2 / (sigma^2 + sigma0^2)

truePosteriorSigma :: Double
truePosteriorSigma = sqrt $ recip (recip sigma0^2 + recip sigma^2)
\end{code}

This gives $\hat{\mu} = \eval{truePosteriorMu}$ and $\hat{\sigma} = \eval{truePosteriorSigma}$.

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
\end{code}

\section{Posteriors}

For us, we want the posterior

$$
\varpi(\mu) = \frac{1}{Z}\exp{\frac{(x - \mu)^2}{2\sigma^2}} \exp{\frac{(\mu - \mu_0)^2}{2\sigma_0^2}}
$$

where $x, \mu_0, \sigma$ and $\sigma_0$ are all given but $Z$ is unknown.

\subsection{Random Walk Metropolis}

Let's implement a traditional random walk. Here's the proposal distribution:

\begin{code}
qq :: Double -> Double -> Double
qq w w' = exp (-(w - w')^2 / (2 * sigmaP^2))
\end{code}

And here's the specification for $\rho$:

\begin{code}
testRho :: (a -> Double) -> (a -> b -> Double) -> (a, b) -> Double
testRho varphi q (w, w') = varphi w * q w w'
\end{code}

Here's the un-normalised posterior:

\begin{code}
testVarphi :: Floating a => a -> a
testVarphi mu = exp (-(z - mu)^2 / (2 * sigma^2)) * exp (-(mu - mu0)^2 / (2 * sigma0^2))
\end{code}

We can now use one step of the algorithm and then run it for as many times as we wish:

\begin{code}
testRwmOneStep :: MonadDistribution m => (Double, Double) -> m (Double, Double)
testRwmOneStep (xi0, _) = algo1 (xi0, undefined) condDistXiMinus0GivenXi0 phi a rho
  where
    phi = \(x, y) -> (y, x)
    a   = min 1.0
    rho = testRho testVarphi qq
    condDistXiMinus0GivenXi0 = \xi00 -> normal xi00 sigmaP

testRwm :: (Eq a, Num a, MonadDistribution m) =>
               a -> m [(Double, Double)]
testRwm n = unfoldM f (n, (1.0, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- testRwmOneStep s
                  return $ Just (s, (m - 1, x))
\end{code}

And we can see the results in Figure~\ref{fig:Rwm}. A bit skewed but
we didn't burn in and the starting value is 1.0.

\begin{figure}[!htbp]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/RwM.png}
    \caption{Random Walk Metropolis}
    \label{fig:Rwm}
\end{figure}

\subsection{What monad-bayes does}

Here's what I think monad-bayes does. The results are in Figure~\ref{fig:Mb}.

\todo{I have yet to verify this! And I don't see the $(x, y) \mapsto (x + y, -y)$ in the monad-bayes code. But if I use $(x, y) \mapsto (y, x)$ then I don't get the intended result and indeed this in Example 6 in~\cite{9117ec7157f141e28e5fe7bc575df48b}}

\todo[color=green!40]{I now know that monad-bayes does not use this but it is interesting nonetheless}

\begin{code}
testMbOneStep :: MonadDistribution m => (Double, Double) -> m (Double, Double)
testMbOneStep (xi0, _) = algo1 (xi0, undefined) condDistXiMinus0GivenXi0 phi a rho
  where
    phi = \(x, y) -> (x + y, -y)
    a   = min 1.0
    rho = testRho testVarphi (\_ -> \_ -> 1.0)
    condDistXiMinus0GivenXi0 = const (quantile (normalDistr 0.0 1.0) <$> random)

testMb :: (Eq a, Num a, MonadDistribution m) =>
               a -> m [(Double, Double)]
testMb n = unfoldM f (n, (1.0, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- testMbOneStep s
                  return $ Just (s, (m - 1, x))
\end{code}

\begin{figure}[!htbp]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/MB.png}
    \caption{monad-bayes}
    \label{fig:Mb}
\end{figure}

\section{Notes to be tidied}

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

\section{Student's T}

Let's try running it on Student's T with 5 degrees of freedeom using
what I hope the textbook presentation of Metropolis---Hastings. The
probability density function (aka the Radon-Nikodym derivative wrt
Lebesgue measure) is

$$
f(t)=\frac{8}{3 \pi \sqrt{5}\left(1+\frac{t^2}{5}\right)^3}
$$

It's traditional to have $q_z(\cdot) \sim {\mathcal{N}}(z, \sigma_p^2)$ for some given $\sigma_p$.

Here's the density function for Student's T with 5 degrees of
freedom. We've defined it in terms of an un-normalised density so that
we can pretend we don't know the normalisation constant but still
sample from the distribution via MCMC.

\begin{code}
student5U :: Floating a => a -> a
student5U t =  1 / (1 + t^2/5)^3

student5 :: Floating a => a -> a
student5 t = student5U t * 8 / (3 * pi * sqrt 5)
\end{code}

Again, we can now use one step of the algorithm and then run it for as
many times as we wish. We instantiate the algorithm to be a Random
Walk Metropolis.

\begin{code}
testStudentRwmOneStep :: MonadDistribution m =>
                         (Double, Double) -> m (Double, Double)
testStudentRwmOneStep (xi0, _) = algo1 (xi0, undefined) (\xi00 -> normal xi00 sigmaP)
                                       (\(x, y) -> (y, x)) (min 1.0) (testRho student5U qq)

testStudentRwm :: (Eq a, Num a, MonadDistribution m) =>
                 a -> m [(Double, Double)]
testStudentRwm n = unfoldM f (n, (0.0, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- testStudentRwmOneStep s
                  return $ Just (s, (m - 1, x))
\end{code}

\begin{code}
testStudentMbOneStep :: MonadDistribution m => (Double, b) -> m (Double, Double)
testStudentMbOneStep (xi0, _) = algo1 (xi0, undefined) (const ((quantile (studentT 5)) <$> random))
                                      (\(x, y) -> (x + y, -y)) (min 1.0) (student5U . fst)

testStudentMb :: (Eq a, Num a, MonadDistribution m) =>
                 a -> m [(Double, Double)]
testStudentMb n = unfoldM f (n, (0.0, 0.0 / 0.0))
  where
    f (0, _) = return Nothing
    f (m, s) = do x <- testStudentMbOneStep s
                  return $ Just (s, (m - 1, x))
\end{code}

The results are shown in Figure~\ref{fig:StudRwm} and Figure~\ref{fig:StudMb}.

\begin{figure}[!htbp]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/StudRwm.png}
    \caption{Random Walk Metropolis Student's T 5}
    \label{fig:StudRwm}
\end{figure}

\begin{figure}[!htbp]
    \centering
    \includegraphics[width=0.8\textwidth]{diagrams/StudMb.png}
    \caption{Monad Bayes Student's T 5}
    \label{fig:StudMb}
\end{figure}

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
    f (m, s) = do a <- algo1 s (const $ normal 0.0 1.0) testPhi (min 1.0) (expTestRhoHMC' student5U)
                  return $ Just (s, (m - 1, a))

writeToFile :: (Int -> SamplerIO [(Double, Double)]) -> FilePath -> Int -> IO ()
writeToFile t p n = do
  rs <- sampler (t n)
  let ss = zip ((map show [1 :: Int ..]) :: [String]) (map fst rs)
  BL.writeFile (p <> (show $ n) <> ".csv") $ encode $
    foldMap encodeRecord ss

main :: IO ()
main = do
  writeToFile reallyTestR''  "StudSampsHMC"    1000
  writeToFile testStudentRwm "StudSampsRwm"  100002
  writeToFile testStudentMb  "StudSampsMb"    10003
  writeToFile testRwm        "Rwm"           100000
  writeToFile testMb         "Mb"            100000
\end{code}

\todo{It should make no difference what we return in the momentum
position; yet it does? Maybe not but at least investigate it.}

\section{Whatever}

%if style == newcode
\begin{code}
bigU :: Floating a => a -> a
bigU = negate . log . student5U


gradU' :: Floating a => a -> a
gradU' w = case grad (\[x] -> bigU x) $ [w] of
             [y] -> y
             _   -> error "Whatever"
\end{code}
%endif

\section{Bibliography}

\bibliographystyle{plain}
\bibliography{references.bib}

\end{document}
