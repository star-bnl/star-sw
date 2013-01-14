#ifndef STAR_DAQ_L4_H_
#define STAR_DAQ_L4_H_
# ifndef _DAQ_L4_H_
// L4 header to separate the "daq data" and "daq reader"
#  ifndef DAQ_L4_DATA_STRUCTURE
#    define DAQ_L4_DATA_STRUCTURE
#      include "daq_l4.h"
#    undef  DAQ_L4_DATA_STRUCTURE
#  endif   //DAQ_L4_DATA_STRUCTURE 
# endif    //_DAQ_L4_H_
#endif     // STAR_DAQ_L4_H_
