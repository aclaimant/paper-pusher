FROM clojure

# Create code directory
RUN mkdir -p /src/aclaimant/paper-pusher
WORKDIR /src/aclaimant/paper-pusher

# Copy all of the code
COPY . /src/aclaimant/paper-pusher

EXPOSE 3000
EXPOSE 7005

CMD lein ring server-headless
