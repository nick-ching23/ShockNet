<h1 align="center">Shocknet</h1>
<p align="center">
Collaborators: Nick Ching, Erica Choi
</p>

Shocknet is a Haskell-based implementation of the Independent Cascade Model (ICM) for information diffusion on networks, optimized with parallelization techniques. By leveraging Haskell's powerful concurrency and parallel libraries, Shocknet accelerates simulation runs across large graphs, making it ideal for researchers and practitioners in computational social science, network theory, and graph analytics.

## Introduction

Information often spreads through social networks in patterns that can be modeled and analyzed. The Independent Cascade Model is one such model used to simulate how an idea, behavior, or "infection" (e.g., adoption of a product) propagates across a network. With Shocknet, you can run large-scale simulations more efficiently by exploiting the parallel computing capabilities provided by Haskell's runtime and libraries.

## Background: Independent Cascade Model

The Independent Cascade Model is a discrete-time probabilistic model of diffusion:

1. The process begins with a set of "seed" nodes that are initially active (influenced).
2. At each time step, any newly activated node attempts to influence its neighbors.
3. Each influence attempt succeeds independently with a given probability.
4. Once a node becomes active, it remains active indefinitely.

Simulations often involve running multiple cascades across large networks. However, as networks grow, the computational demands can become substantial.

## Key Features

- **Parallel Execution:** Exploit multi-core architectures to significantly reduce simulation time (currently 7x speedup).
- **High-Level Functional Codebase:** Written in Haskell, providing a concise, maintainable, and reliable code structure.

## Getting Started

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) (The Glasgow Haskell Compiler)
- [Stack](https://docs.haskellstack.org/en/stable/README/) for building the project
- A multi-core processor for optimal parallel performance

### Installation

Clone the repository and build using Stack:

```bash
git clone https://github.com/nick-ching23/ShockNet.git shocknet
cd shocknet
stack ghc -- -O2 -threaded -rtsopts parallel_icm.hs
```

Running the project:

```bash
time ./parallel_icm +RTS -N<your number of cores> -s
```
