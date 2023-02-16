FROM clojure:openjdk-17-lein

WORKDIR /app

COPY project.clj .
RUN lein deps

COPY . .
RUN lein ring uberjar

CMD cat config.edn && java $JAVA_OPTS -XX:MaxMetaspaceSize=512m \
  -XX:-OmitStackTraceInFastThrow -XX:+PreserveFramePointer \
  -Dconfig.edn=config.edn \
  -jar target/paper-pusher-standalone.jar
