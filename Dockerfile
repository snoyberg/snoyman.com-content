FROM snoyberg/snoyman.com:72ad7d7f402f25a13a0cc899c7f9899a466080eb

RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev \
  netbase

ENV PORT 3000
WORKDIR /app
CMD ["/usr/local/bin/snoymancom"]

COPY . /app/content
