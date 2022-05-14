FROM clojure

EXPOSE 8999
EXPOSE 62222

RUN apt-get update && apt-get install -y graphviz 

COPY ./project.clj ./profiles.clj ./welcome.clj /usr/src/app/

COPY ./src /usr/src/app/src

WORKDIR /usr/src/app

RUN lein deps

CMD lein gorilla :ip 0.0.0.0 :port 8999 :nrepl-port 62222

