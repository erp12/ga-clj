# Contributing to ga-clj

All interest in ga-clj, as a user or contributor, is greatly appreciated! This document
will go into detail on how to contribute to the development of ga-clj.

## Before Contributing

Before reading further we ask that you read our [Code of Conduct](https://github.com/erp12/ga-clj/blob/master/CODE_OF_CONDUCT.md)
which will be enforced by the maintainers in order to ensure that development of ga-clj stays focused and productive.

If you are new to contributing to open source, or GitHub, the following links may be helpful starting places:

- [How to Contribute to Open Source](https://opensource.guide/how-to-contribute/)
- [Understanding the GitHub flow](https://guides.github.com/introduction/flow/index.html)

### We Use Github Flow

This means that all code and documentation changes happen through pull requests. We actively welcome your pull requests.
We highly recommend the following workflow.

1. Fork the repo and create your branch from `master`.
2. If you've added code that should be tested, add tests.
3. If you've changed APIs, update the documentation.
4. Ensure the test suite passes.
5. Create the pull request.

### Any contributions you make will be under the MIT Software License

In short, when you submit code changes, your submissions are understood to be under the same [MIT License](http://choosealicense.com/licenses/mit/) that covers the project.
Feel free to contact the maintainers if that's a concern.

# How to contribute a ...

## Bug Report

We use GitHub issues to track public bugs. Report a bug by [opening a new issue]().

**Great Bug Reports** tend to have at least the following:

- A quick summary and/or background
- The steps to reproduce.
- When possible, minimal code that reproduces the bug.
- A description of what you expected versus what actually happens.

## Feature Request

We like to hear in all feature requests and discussion around the direction of the project. The best place
to discuss future features is the project's [discussion page](https://github.com/erp12/ga-clj/discussions) under
the [ga-clj](https://github.com/erp12/ga-clj/discussions/categories/ga-clj) category.

## Bug fix, new feature, documentation improvement, or other change.

We welcome contribution to the codebase via pull requests. In most cases, it is beneficial to discuss your change
with the community via a GitHub issue or discussion before working on a pull request. Once you decide to work on a
pull request, please follow the workflow outlined in the above sections.

Once you open the pull request, it will be tested with by CI and reviewed by other contributors (including at least one
project maintainer). After all iterations of review are finished, one of the project maintainers will merge
your pull request.

## Running Tests

When working on a code change or addition to ga-clj, it is expected that all changes 
pass existing tests and probably introduce new tests to ensure stability of future changes. 
To run the test suite against Clojure, run the following command in the project root directory
 using the [Clojure CLI](https://clojure.org/guides/deps_and_cli).

```commandline
clj -T:build test-clj   
```

The ga-clj library supports both Clojure and Clojurscript, and thus the test suite is expected to pass on
both platforms. To run the test suite against Clojurescript, first [install node.js](https://nodejs.org/en/download/package-manager/)
and then use the following command.

```commandline
clj -T:build test-cljs
```

The project contains a number of example files as a form of executable documentation. After making a 
change, we check the example files have not been broken with the following command.

```commandline
clj -T:build examples
```

For convenience, we often run the test suite on both Clojure and Clojurescript, as well as the
examples, using a single command:

```commandline
clj -T:build ci
```
