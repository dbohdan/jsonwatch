# Based on
# https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack

FROM alpine:3.6

RUN echo http://dl-cdn.alpinelinux.org/alpine/v3.6/community \
         >> /etc/apk/repositories
RUN apk update
RUN apk add alpine-sdk ghc gmp-dev libffi-dev zlib-dev

ADD https://s3.amazonaws.com/static-stack/stack-1.1.2-x86_64 \
    /usr/local/bin/stack
RUN echo 1b60060768f4644e1446d9e669a1b00a063c045339fb7a6863bb092a93070c53 \
         ' /usr/local/bin/stack' > checksum \
    && sha256sum -c checksum \
    && rm checksum
RUN chmod 755 /usr/local/bin/stack

ADD ./ /usr/src/jsonwatch
WORKDIR /usr/src/jsonwatch
