ARG starenv=root616

FROM ghcr.io/star-bnl/star-spack:v0.1.3-${starenv}

ENV NODEBUG=yes
ENV STAR=/star-sw
ENV STAR_LIB=$STAR/.${STAR_HOST_SYS}/LIB
ENV STAR_BIN=$STAR/.${STAR_HOST_SYS}/BIN
ENV STAR_SYS=x8664_sl7
ENV PATH=$STAR_BIN:$STAR/mgr:$PATH
ENV LD_LIBRARY_PATH=$STAR_LIB:$LD_LIBRARY_PATH
ENV ROOT_INCLUDE_PATH=$STAR/.${STAR_HOST_SYS}/include

COPY . /star-sw

RUN cd /star-sw \
 && source /etc/profile \
 && module load vc_-0.7.4 \
 && MYSQL=$(dirname $(mysql_config --variable=pkgincludedir)) \
    LIBXML2_DIR=$(xml2-config --prefix) \
    GSL_DIR=$(gsl-config --prefix) \
    FASTJET_DIR=$(fastjet-config --prefix) \
    cons \
 && find /star-sw/.$STAR_HOST_SYS -name *.o -exec rm '{}' \;

RUN install /star-sw/StRoot/macros/.rootrc .

RUN echo -e '#!/bin/bash --login\n set -e; eval "$@"' > entrypoint.sh && chmod 755 entrypoint.sh
ENTRYPOINT ["./entrypoint.sh"]
CMD ["/bin/bash"]
