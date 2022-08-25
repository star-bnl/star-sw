# syntax=docker/dockerfile:latest

ARG starenv=root5

# Pick one from [gcc485, gcc11]
ARG compiler=gcc485

FROM ghcr.io/star-bnl/star-spack:v0.1.7-${starenv}-${compiler}

ENV NODEBUG=yes
ENV STAR=/star-sw
ENV STAR_LIB=$STAR/.${STAR_HOST_SYS}/LIB
ENV STAR_BIN=$STAR/.${STAR_HOST_SYS}/BIN
ENV STAR_SYS=x8664_sl7
ENV PATH=$STAR_BIN:$STAR/mgr:$PATH
ENV LD_LIBRARY_PATH=$STAR_LIB:$LD_LIBRARY_PATH
ENV ROOT_INCLUDE_PATH=$STAR/.${STAR_HOST_SYS}/include

WORKDIR ${STAR}
COPY . ${STAR}

SHELL ["/bin/bash", "-l", "-c"]

RUN cons \
 && find .$STAR_HOST_SYS -name *.o -exec rm '{}' \;

COPY --chmod=0755 <<-"EOF" /opt/entrypoint.sh
	#!/bin/bash -l
	set -e
	install $STAR/StRoot/macros/.rootrc .
	exec "$@"
EOF

ENTRYPOINT ["/opt/entrypoint.sh"]
CMD ["/bin/bash"]
