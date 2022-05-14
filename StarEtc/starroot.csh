setup 64bits
setenv ROOT_LEVEL 6.24.00

module use /cvmfs/star.sdcc.bnl.gov/star-spack/spack/share/spack/modules/linux-rhel7-x86_64
module load star-env-root-${ROOT_LEVEL}

setenv STARSYS ${STAR_LIB:h}
setenv STAR_MGR $STAR/mgr


echo ROOTSYS = $ROOTSYS
echo STARSYS = $STARSYS

exit 0

