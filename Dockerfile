FROM node:14.7-stretch AS node-deps
WORKDIR /app
COPY package.json package.json
RUN npm install

FROM clojure:openjdk-11-tools-deps AS build
RUN apt-get update && apt-get install -y tini # handle SIGINT and SIGTERM properly
WORKDIR /app
COPY --from=node-deps /app/node_modules /app/node_modules
COPY deps.edn deps.edn
RUN clojure -A:demo -M -e :ok        # preload deps
RUN clojure -M:build info

COPY shadow-cljs.edn shadow-cljs.edn
COPY src src
COPY src-docs src-docs
COPY resources resources
ARG VERSION
RUN clojure -M:build release demo --config-merge "{:closure-defines {hyperfiddle.electric-client/VERSION \"$VERSION\"}}"

ENTRYPOINT ["/usr/bin/tini", "--"] # will get PID 1 and handle SIGINT and SIGTERM
ENV VERSION=$VERSION
CMD clj -J-DHYPERFIDDLE_ELECTRIC_SERVER_VERSION=$VERSION -A:demo -M -m user
