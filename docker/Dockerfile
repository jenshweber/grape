FROM neo4j:3.5.30
RUN apt-get update && apt-get install -y \
    graphviz \
    leiningen
COPY project.clj profiles.clj /var/lib/neo4j
COPY scripts/docker-entrypoint.sh /
COPY book/welcome.clj /var/lib/neo4j

