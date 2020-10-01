FROM alpine:3.12 as builder

# install ghc and stack
RUN \
  apk add --no-cache git curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl && \
  apk add --no-cache zlib zlib-dev zlib-static ncurses-static libsodium libsodium-dev libsodium-static && \
  curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
  chmod +x /usr/bin/ghcup && \
  ghcup -v install ghc 8.6.5 && \
  ghcup -v set ghc 8.6.5 && \
  ghcup -v install cabal

COPY . /app

# install app
RUN \
  mkdir -p ~/.local/bin && \
  export PATH="/root/.ghcup/bin:$PATH" && \
  cabal update && \
  cd /app && \
  scripts/gen-cabal-nosystemd.sh && \
  cabal install --project-file=cabal.nosystemd.project --installdir="$HOME/.local/bin" --install-method=copy --overwrite-policy=always --ghc-options='-split-sections -optl-static'

# strip binary
RUN strip -s /root/.local/bin/cardano-tx

FROM alpine:3.12

COPY --from=builder /root/.local/bin/cardano-tx /usr/bin/cardano-tx

ENTRYPOINT ["/usr/bin/cardano-tx"]
