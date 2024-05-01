FROM debian:bookworm-20230703-slim

# odpic libs are in contrib
RUN echo "deb http://deb.debian.org/debian/ bookworm contrib" > /etc/apt/sources.list

RUN (export DEBIAN_FRONTEND=noninteractive && \
     apt-get update && \
     apt-get install -y --no-install-recommends netbase \
                                                ca-certificates \
                                                libaio1 \
                                                wget \
                                                unzip \
                                                libodpic4 \
                                                libpq5 \
    && rm -r /var/lib/apt/lists /var/cache/apt/archives \
    )

ARG ORACLE_VERSION

# instant client
RUN wget "https://download.oracle.com/otn_software/linux/instantclient/$(echo "${ORACLE_VERSION}" | cut -d '.' -f 1,2 | tr -d '.')0000/instantclient-basic-linux.x64-${ORACLE_VERSION}.zip"

RUN unzip "instantclient-basic-linux.x64-${ORACLE_VERSION}.zip" -d /opt/oracle && \
    rm "instantclient-basic-linux.x64-${ORACLE_VERSION}.zip"

COPY ./functional-tests .
