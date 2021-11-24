# GA-CLJ

A genetic algorithm framework in Clojure that makes minimal assumptions.

## Rationale

@todo write me!

Outline
- prior art
  - Clojush, Propeller, link to lspector Conj talk. Maybe link to famous PushGP uses (quantum, finite algebra)
    - Coupled to PushGP.
  - Other JVM tools: ECJ, jenetics, etc.
    - Other tools make assumptions about genome/phenome structures.
    - Not easily extended via interop.
- Decoupling from assumptions
  - Genomes can be any data.
  - Phenomes/individuals are open maps that hold genomes, errors, and anything else the user wants to add to them.
  - "breeding" new genomes is done via a user supplied function. 
- Solving common issues
  - Ensure same, well tested, implementations of common algorithms. (aka toolbox)
  - Difficult to debug evolution because it is random.
      - GA-CLJ enhances exceptions with additional data (for example, the genome that caused the problem.)

## Installation

We recommend declaring your ga-clj dependency using git coordinates. Add the following to your `deps.edn`.

```clojure
;; Add to 
{io.github.erp12/ga-clj {:git/tag "0.0.1" :git/sha "7cec443"}}
```

In the future, we may also publish releases to Clojars.

## Guide

Current ga-clj only supports generational genetic algorithms.

### Terminology

- `genome-factory`: A nullary function for creating random genomes.
- `genome->phenome`: A function from genome to a "phenome" map containing additional used to drive breeding. 
    Often this map contains the errors/fitness associated with the genome but could also contain any other values.
- `breed`: A function that takes the current population of individuals as input and returns a new child genome.
    Often this function will perform parent selection and variation operators. The `erp12.ga-clj.toolbox` namespace
    provides implementations of commonly used algorithms that will likely be useful to call in breed functions.

Additional terminology and configuration parameters can be found in the docstring of `evolve` functions found
in namespaces that provide a specific kind of genetic algorithm. For example:

- `erp12.ga-clj.generational/evolve`

## Examples

See the `examples/` directory. You can run the examples on the command line. For example:

```text
clj -M:examples -m erp12.ga-clj.examples.alphabet
```

Change the namespace in the command to run a different example.

## Contributing

See the `CONTRIBUTING.md` for more information, including how to run tests.

## To-Do before "official" release

- Fill out the toolbox with other common error functions, selection methods, and variation operators.
- Add more examples that test the design/abstraction in a wider range of scenarios.
  - TSP?
  - Knapsack problem?
- Figure out how best to handle logging, monitoring, data collection, etc. 
  - In the library or in user code?
- Rationale and Guide
