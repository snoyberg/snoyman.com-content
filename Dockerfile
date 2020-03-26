FROM snoyberg/snoyman.com:c4bcfad9772e15e106aa38c96897aec8fa83b305

RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev \
  netbase

ENV PORT 3000
WORKDIR /app
CMD ["/usr/local/bin/snoymancom"]

COPY . /app/content
