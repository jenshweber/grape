FROM clojure

EXPOSE 8999
EXPOSE 62222

RUN apt-get update && apt-get install -y graphviz 

COPY project.clj /usr/src/app/

COPY help /usr/src/app/help

COPY resources /usr/src/app/resources

COPY src /usr/src/app/src

WORKDIR /usr/src/app

RUN lein deps

CMD lein run

