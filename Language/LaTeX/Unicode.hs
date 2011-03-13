{-# LANGUAGE FlexibleInstances #-}
module Language.LaTeX.Unicode where

import Language.LaTeX.Types
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Math as M

{-
      α = alpha)
      β = beta)
      χ = chi)
      δ = delta)
      Δ = _Delta)
      ε = epsilon)
    --  = varepsilon)
      η = eta)
      γ = gamma)
      Γ = _Gamma)
      ι = iota)
      κ = kappa)
      λ = lambda)
      Λ = _Lambda)
      μ = mu)
      ν = nu)
      ω = omega)
      Ω = _Omega)
      ο = omicron)
      φ = phi)
    --  = varphi)
      Φ = _Phi)
      π = pi)
      Π = _Pi)
      ψ = psi)
      ρ = rho)
      σ = sigma)
      Σ = _Sigma)
      τ = tau)
      θ = theta)
    --  = vartheta)
      Θ = _Theta)
    --  = upsilon)
      ξ = xi)
    --  = _Xi)
      ζ = zeta)
-}

class Greek a where
    -- varepsilon
    -- varphi
    -- vartheta
    -- upsilon
    -- _Xi
  α  :: a
  β  :: a
  χ  :: a
  δ  :: a
  _Δ :: a
  ε  :: a
  η  :: a
  γ  :: a
  _Γ :: a
  ι  :: a
  κ  :: a
  λ  :: a
  _Λ :: a
  μ  :: a
  ν  :: a
  ω  :: a
  _Ω :: a
  ο  :: a
  φ  :: a
  _Φ :: a
  π  :: a
  _Π :: a
  ψ  :: a
  ρ  :: a
  σ  :: a
  _Σ :: a
  τ  :: a
  θ  :: a
  _Θ :: a
  ξ  :: a
  ζ  :: a

instance Greek MathItem where
  α  = M.alpha
  β  = M.beta
  χ  = M.chi
  δ  = M.delta
  _Δ = M._Delta
  ε  = M.epsilon
  η  = M.eta
  γ  = M.gamma
  _Γ = M._Gamma
  ι  = M.iota
  κ  = M.kappa
  λ  = M.lambda
  _Λ = M._Lambda
  μ  = M.mu
  ν  = M.nu
  ω  = M.omega
  _Ω = M._Omega
  ο  = M.omicron
  φ  = M.phi
  _Φ = M._Phi
  π  = M.pi
  _Π = M._Pi
  ψ  = M.psi
  ρ  = M.rho
  σ  = M.sigma
  _Σ = M._Sigma
  τ  = M.tau
  θ  = M.theta
  _Θ = M._Theta
  ξ  = M.xi
  ζ  = M.zeta

{-
  α  = α
  β  = β
  χ  = χ
  δ  = δ
  _Δ = _Δ
  ε  = ε
  η  = η
  γ  = γ
  _Γ = _Γ
  ι  = ι
  κ  = κ
  λ  = λ
  _Λ = _Λ
  μ  = μ
  ν  = ν
  ω  = ω
  _Ω = _Ω
  ο  = ο
  φ  = φ
  _Φ = _Φ
  π  = π
  _Π = _Π
  ψ  = ψ
  ρ  = ρ
  σ  = σ
  _Σ = _Σ
  τ  = τ
  θ  = θ
  _Θ = _Θ
  ξ  = ξ
  ζ  = ζ
-}

instance Greek (LatexM LatexItm) where
  α = B.math M.alpha
  β = B.math M.beta
  χ = B.math M.chi
  δ = B.math M.delta
  _Δ = B.math M._Delta
  ε = B.math M.epsilon
  η = B.math M.eta
  γ = B.math M.gamma
  _Γ = B.math M._Gamma
  ι = B.math M.iota
  κ = B.math M.kappa
  λ = B.math M.lambda
  _Λ = B.math M._Lambda
  μ = B.math M.mu
  ν = B.math M.nu
  ω = B.math M.omega
  _Ω = B.math M._Omega
  ο = B.math M.omicron
  φ = B.math M.phi
  _Φ = B.math M._Phi
  π = B.math M.pi
  _Π = B.math M._Pi
  ψ = B.math M.psi
  ρ = B.math M.rho
  σ = B.math M.sigma
  _Σ = B.math M._Sigma
  τ = B.math M.tau
  θ = B.math M.theta
  _Θ = B.math M._Theta
  ξ = B.math M.xi
  ζ = B.math M.zeta
