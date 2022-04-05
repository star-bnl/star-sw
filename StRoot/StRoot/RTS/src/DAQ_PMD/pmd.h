#ifndef STAR_DAQ_PMD_H_
#define STAR_DAQ_PMD_H_
# ifndef _DAQ_PMD_H_
// PMD header to separate the "daq data" and "daq reader"
// The better solution is not move the C-struct in this file
// and remove the confusing ifdef CPP macro
#  ifndef DAQ_PMD_DATA_STRUCTURE
#    define DAQ_PMD_DATA_STRUCTURE
#      include "daq_pmd.h"
#    undef  DAQ_PMD_DATA_STRUCTURE
#  endif   //DAQ_PMD_DATA_STRUCTURE 
# endif    //_DAQ_PMD_H_
#endif     // STAR_DAQ_PMD_H_
