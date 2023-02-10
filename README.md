# paper-pusher

A Clojure library designed to ... well, that part is up to you.

## Usage

## Release

Here's how we release paper pusher (assuming we ever do it again):

```
docker build -t aclaimant/paper-pusher,aclaimant/paper-pusher:$(date "+%Y%m%d") .
docker push aclaimant/paper-pusher -a
```
