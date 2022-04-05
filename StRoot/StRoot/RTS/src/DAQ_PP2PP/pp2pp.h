#ifndef STAR_DAQ_PP2PP_H_
#define STAR_DAQ_PP2PP_H_
# ifndef _DAQ_PP2PP_H_
// PP2PP header to separate the "daq data" and "daq reader"
#  ifndef DAQ_PP2PP_DATA_STRUCTURE
#    define DAQ_PP2PP_DATA_STRUCTURE
#      include "daq_pp2pp.h"
#    undef  DAQ_PP2PP_DATA_STRUCTURE
#  endif   //DAQ_PP2PP_DATA_STRUCTURE 
# endif    //_DAQ_PP2PP_H_
#endif     // STAR_DAQ_PP2PP_H_
