FROM debian:stable-slim

# /home/dev
RUN mkdir /home/dev
WORKDIR /home/dev
ENV HOME=/home/dev
RUN mkdir bin tools
ENV PATH="$HOME/bin:${PATH}"

# Utils
RUN apt update && apt install -y zip sed util-linux git nano make gcc mingw-w64

# ACME for Linux/Win32
WORKDIR /home/dev/tools
RUN git clone https://github.com/meonwax/acme.git && \
    make -C acme/src && cp acme/src/acme $HOME/bin && cp acme/src/acme acme/ && \
    (cat acme/src/Makefile | sed 's/gcc/x86_64-w64-mingw32-gcc/' > acme/src/Makefile.win32) && \
    (make -C acme/src -f Makefile.win32 clean acme|| true) && \
    x86_64-w64-mingw32-strip -o acme/acme.exe acme/src/acme.exe && \
    rm -rf acme/src

WORKDIR /home/dev

CMD /bin/bash
