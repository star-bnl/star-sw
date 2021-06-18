FROM centos:7.4.1708 AS build-stage

WORKDIR /workdir

SHELL ["/bin/bash", "-c"]
ENV LC_ALL=en_US.UTF-8

# epel repo is for python pip only
RUN yum update -q -y \
 && yum install -y epel-release \
 && yum install -y \
    binutils gcc gcc-c++ gcc-gfortran xorg-x11-proto-devel \
    git bzip2 unzip file which make imake patch \
    bison byacc flex flex-devel libcurl-devel \
    perl perl-Data-Dumper perl-Env perl-Digest-MD5 \
    lapack-static blas-static \
    libX11-devel libXext-devel libXpm-devel openmotif-devel \
    python python-pip \
 && yum clean all

# Install extra python modules used by the STAR software
RUN pip install pyparsing

ADD https://api.github.com/repos/star-bnl/star-spack/commits/main star-spack-ref.json
RUN git clone --recurse-submodules https://github.com/star-bnl/star-spack.git

RUN source star-spack/setup.sh \
 && spack env create mydocker star-spack/environments/star-x86_64-root534-docker.yaml \
 && spack env activate mydocker \
 && spack install -j 5 --fail-fast \
 && spack gc -y

# Install CERNLIB
RUN mkdir /cern && cd /cern \
 && curl -sL https://github.com/psilib/cernlib/archive/centos7.tar.gz | tar -xz --strip-components 1 \
 && ./build_cernlib.sh \
 && cd /cern/2006/lib \
 && ln -s /usr/lib64/libblas.a libblas.a \
 && ln -s /usr/lib64/liblapack.a liblapack3.a \
 && cd /cern \
 && ln -s 2006 pro \
 && rm -fr /cern/2006/src /cern/2006/build /tmp/*

ENV STAR=/star-sw
ENV USE_64BITS=1
ENV CERN=/cern
ENV CERN_LEVEL=pro
ENV CERN_ROOT=$CERN/$CERN_LEVEL
ENV OPTSTAR=/opt/view
ENV STAR_CVS_REF=YYY
ENV STAR_HOST_SYS=sl88_gcc789
ENV NODEBUG=yes
ENV STAR_LIB=$STAR/.${STAR_HOST_SYS}/LIB
ENV STAR_BIN=$STAR/.${STAR_HOST_SYS}/BIN
ENV STAR_SCRIPTS=$STAR/scripts
ENV STAR_CGI=$STAR/cgi
ENV STAR_MGR=$STAR/mgr
ENV STAR_PAMS=$STAR/pams
ENV STAR_LEVEL=$STAR_CVS_REF
ENV STAR_VERSION=$STAR_CVS_REF
ENV STAR_SYS=x8664_sl7
ENV PATH=$CERN_ROOT/bin:$STAR_BIN:$STAR_MGR:$PATH
ENV LD_LIBRARY_PATH=$STAR_LIB:$LD_LIBRARY_PATH
ENV LIBPATH+=":/lib64:/lib"

# Dummy directories checked by cons
RUN mkdir $OPTSTAR/lib && mkdir $OPTSTAR/include

COPY . /star-sw

RUN source star-spack/setup.sh && spack env activate mydocker \
 && export MYSQL=`spack location --install-dir mysql` \
 && export LIBXML2_DIR=`spack location --install-dir libxml2` \
 && cd /usr/include && ln -s $MYSQL/include mysql \
 && cd /star-sw \
 && cons +asps/staf \
 && cons +asps/Simulation/agetof \
 && cons +StarVMC/Geometry \
 && cons %GeoTestMaker %Kinematics %OnlTools %StAngleCorrMaker %StDaqClfMaker \
    %StEEmcPool %StEbye2ptMaker %StEbyePool %StEbyeScaTagsMaker %StFgtPool \
    %StFtpcV0Maker %StHighptPool %StJetFinder %StRoot/Stv %StRoot/StvMaker \
    %StRoot/StvSeed %StShadowMaker %StSpinMaker %StSpinPool %StStrangePool \
    %pams/sim/g2r %StRoot/StHbtMaker
