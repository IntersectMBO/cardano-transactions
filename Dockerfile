FROM alpine:3.12 as builder

# install ghc and stack
RUN \
  apk add --no-cache git curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl && \
  apk add --no-cache zlib zlib-dev zlib-static ncurses-static libsodium libsodium-dev libsodium-static && \
  curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
  chmod +x /usr/bin/ghcup && \
  ghcup install ghc 8.6.5 && \
  ghcup set ghc 8.6.5 && \
  curl -L -s https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz && \
  mv stack-*/stack /usr/bin/stack && \
  rm -rf stack-* && \
  chmod +x /usr/bin/stack

COPY . /app

# install cardano-tx
RUN \
  cd /app && \
  export PATH="/root/.ghcup/bin:$PATH" && \
  stack --system-ghc install --flag 'cardano-node:-systemd' --flag 'cardano-config:-systemd' --ghc-options='-optl-static -split-sections' --copy-bins

# strip binary
RUN strip -s /root/.local/bin/cardano-tx

FROM busybox:1.32

COPY --from=builder /root/.local/bin/cardano-tx /usr/bin/cardano-tx

ENTRYPOINT ["/usr/bin/cardano-tx"]
