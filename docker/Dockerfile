FROM clojure

EXPOSE 8999
EXPOSE 62222

RUN apt-get install -y graphviz 

COPY ./project.clj ./profiles.clj ./welcome.clj /usr/src/app

WORKDIR /usr/src/app/

RUN lein deps

CMD lein gorilla :port 8999 :nrepl-port 62222

