<h1 align="center">
  <p align="center">Dynamic Dispatch with Traits</p>
  <img
    src="./img/provider.png?raw=true"
    width='256'
    alt="Logo"
  />
</h1>

<p align="center">
  <a href="https://github.com/mbarbin/provider/actions/workflows/ci.yml"><img src="https://github.com/mbarbin/provider/workflows/ci/badge.svg" alt="CI Status"/></a>
  <a href="https://coveralls.io/github/mbarbin/provider?branch=main"><img src="https://coveralls.io/repos/github/mbarbin/provider/badge.svg?branch=main" alt="Coverage Status"/></a>
  <a href="https://github.com/mbarbin/provider/actions/workflows/deploy-doc.yml"><img src="https://github.com/mbarbin/provider/workflows/deploy-doc/badge.svg" alt="Deploy Doc Status"/></a>
</p>

Provider is an OCaml library for creating Traits and Interfaces. It allows you to define the functionality needed by a library without committing to a specific implementation.

In essence, Provider handles dynamic dispatch, where the target of a function call is not known until runtime. This is particularly useful in situations where there are multiple ways to provide a certain functionality, and the choice of provider is determined by the end user.
