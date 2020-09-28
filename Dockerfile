FROM alpine:3.12 as builder

ARG VERSION=1.0.0

RUN \
  apk add --no-cache tar curl && \
  curl -L -O https://github.com/input-output-hk/cardano-transactions/releases/download/$VERSION/cardano-transactions-$VERSION-linux64.tar.gz && \
  tar -C /usr -xvzf cardano-transactions-$VERSION-linux64.tar.gz bin/cardano-tx && \
  chmod +x /usr/bin/cardano-tx && \
  rm cardano-transactions-$VERSION-linux64.tar.gz

FROM busybox:1.32

COPY --from=builder /usr/bin/cardano-tx /usr/bin/cardano-tx

ENTRYPOINT ["/usr/bin/cardano-tx"]
