# syntax=docker/dockerfile:latest

ARG starenv=root6

# Pick one from [gcc485, gcc11]
# gcc485 is the only one in spack 0.4?
ARG compiler=gcc485

FROM ghcr.io/star-bnl/star-spack:v0.4.0-rc1-${starenv}-${compiler}

ARG compiler

ENV NODEBUG=yes
ENV STAR=/star-sw
ENV STAR_LIB=$STAR/.${STAR_HOST_SYS}/LIB
ENV STAR_BIN=$STAR/.${STAR_HOST_SYS}/BIN
ENV STAR_SYS=x8664_sl7

WORKDIR ${STAR}
COPY . ${STAR}

SHELL ["/bin/bash", "-l", "-c"]

RUN <<EOF
	set -e
	export ROOT_INCLUDE_PATH=$STAR/.${STAR_HOST_SYS}/include:$ROOT_INCLUDE_PATH
	export PATH=$STAR/.${STAR_HOST_SYS}/BIN:$STAR/mgr:$PATH
	export LD_LIBRARY_PATH=$STAR/.${STAR_HOST_SYS}/LIB:$LD_LIBRARY_PATH
	[[ $compiler = "gcc485" ]] && EXTRA_CXXFLAGS="-Werror" || EXTRA_CXXFLAGS=""
	cons EXTRA_CXXFLAGS="$EXTRA_CXXFLAGS"
	find .$STAR_HOST_SYS -name *.o -exec rm '{}' \;
EOF

COPY --chmod=0755 <<-"EOF" /opt/entrypoint.sh
	#!/bin/bash -l
	set -e
	export ROOT_INCLUDE_PATH=$STAR/.${STAR_HOST_SYS}/include:$ROOT_INCLUDE_PATH
	export PATH=$STAR/.${STAR_HOST_SYS}/BIN:$STAR/mgr:$PATH
	export LD_LIBRARY_PATH=$STAR/.${STAR_HOST_SYS}/LIB:$LD_LIBRARY_PATH
	install $STAR/StRoot/macros/.rootrc .
	exec "$@"
EOF

ENTRYPOINT ["/opt/entrypoint.sh"]
CMD ["/bin/bash"]
