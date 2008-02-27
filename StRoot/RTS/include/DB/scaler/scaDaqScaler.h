#ifndef _SCADAQSCALER_H_
#define _SCADAQSCALER_H_

struct scaDaqScaler
{
  //////////////////////
  int idx_rn;                // run number
  int idx_period;            // readout period
  //////////////////////

  unsigned int beginTime;    // unix time for counting period
  unsigned int endTime;     

  int type;                  // RC (jeff) = 1,  DAQ (tonko) == 2, etc...
  int id;
  unsigned int numSeen;
};

#endif
