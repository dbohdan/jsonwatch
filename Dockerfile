# Based on
# https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack

FROM alpine:latest

RUN echo @testing http://nl.alpinelinux.org/alpine/edge/testing \
         >> /etc/apk/repositories
RUN apk update
RUN apk add alpine-sdk ghc@testing gmp-dev libffi-dev zlib-dev

ADD https://s3.amazonaws.com/static-stack/stack-1.1.2-x86_64 /usr/local/bin/stack
RUN chmod 755 /usr/local/bin/stack

ADD ./ /usr/src/jsonwatch
WORKDIR /usr/src/jsonwatch
RUN stack --local-bin-path /usr/local/bin --install-ghc \
          install --test --ghc-options='-optl-static -optl-pthread'
