# Build Docker image.
FROM index.docker.io/library/debian:latest
MAINTAINER qinka
RUN apt-get update && apt-get -y install libgmp10 libpq5 wget curl
ADD bin /usr/bin
CMD start.sh
EXPOSE 3000
