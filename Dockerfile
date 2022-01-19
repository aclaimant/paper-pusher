FROM ubuntu:20.04

RUN apt-get update
RUN apt-get install -y leiningen

WORKDIR /app
COPY . .

ENTRYPOINT lein with-profile +$APP_ENV ring server-headless
