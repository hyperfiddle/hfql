[![hyperfiddle/hfql](https://github.com/hyperfiddle/hfql/actions/workflows/tests.yml/badge.svg?branch=main)](https://github.com/hyperfiddle/hfql/actions/workflows/tests.yml)

# Run JVM tests

```shell
clj -X:test-jvm
```

# Run browser tests (WIP)

```shell
npm install
```

```shell
clj -M:test-cljs complie :browser-test --force-spawn
./node_modules/.bin/karma start --single run
```

# How to run
* `clojure -M:build compile demo` or `clojure -M:build watch demo`
* `clj -A:demo:dev -X user/-main`
