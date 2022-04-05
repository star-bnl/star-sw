#ifndef STAR_DAQ_HLT_H_
#define STAR_DAQ_HLT_H_
# ifndef _DAQ_HLT_H_
// HLT header to separate the "daq data" and "daq reader"
#  ifndef DAQ_HLT_DATA_STRUCTURE
#    define DAQ_HLT_DATA_STRUCTURE
#      include "daq_hlt.h"
#    undef  DAQ_HLT_DATA_STRUCTURE
#  endif   //DAQ_HLT_DATA_STRUCTURE 
# endif    //_DAQ_HLT_H_
#endif     // STAR_DAQ_HLT_H_
