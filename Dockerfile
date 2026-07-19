# syntax=docker/dockerfile:latest

ARG starenv=root6

# Pick one from [gcc485, gcc11]
ARG compiler=gcc485

FROM ghcr.io/yousif-elfeky/star-spack:latest-${starenv}-${compiler}

ARG compiler

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
# This is temporary until I rebuild the container with loading the modules 
RUN echo "module load clhep-2.4.5.1 geant3-3-9-p1-root-6.24.06 geant4-data-10.5.1 || true" >> /etc/profile.d/z11_load_star_deps.sh

RUN <<EOF
	set -e
	[[ $compiler = "gcc485" ]] && EXTRA_CXXFLAGS="" || EXTRA_CXXFLAGS=""
	cons EXTRA_CXXFLAGS="$EXTRA_CXXFLAGS"
	find .$STAR_HOST_SYS -name *.o -exec rm '{}' \;
EOF

COPY --chmod=0755 <<-"EOF" /opt/entrypoint.sh
	#!/bin/bash -l
	set -e
	install $STAR/StRoot/macros/.rootrc .
	exec "$@"
EOF

ENTRYPOINT ["/opt/entrypoint.sh"]
CMD ["/bin/bash"]
