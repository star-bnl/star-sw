#ifndef SUM_EEMC_HTTP
#define SUM_EEMC_HTTP

#include "DSM.hh"

inline void getEemcHTTP(const DSM& dsm, int& httpa, int& httpb, int& httpc)
{
  httpa = ((dsm.channels[0] >> 15) & 0x1) | ((dsm.channels[1] >> 15) & 0x1); // HTTP - 4 o'\clock                                                                                          
  httpb = ((dsm.channels[2] >> 15) & 0x1) | ((dsm.channels[3] >> 15) & 0x1); // HTTP - 6 o'clock                                                                                           
  httpc = ((dsm.channels[4] >> 15) & 0x1) | ((dsm.channels[5] >> 15) & 0x1); // HTTP - 8 o'\clock                                                                                           
}

#endif
