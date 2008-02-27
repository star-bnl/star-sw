#ifndef _SCALXCOUNTS_H_
#define _SCALXCOUNTS_H_

struct scaLxCounts
{
  //////////////////////
  int idx_rn;                // run number
  int idx_level;             // L1, L2, or L3 triggers
  int idx_level_inst;        // L2 and L3 can have multiple entities...
  int idx_trigger;           // which RC trigger
  int idx_period;            // readout period
  //////////////////////

  unsigned int beginTime;    // unix time for counting period
  unsigned int endTime;     

  int numAccepted;           // number accepted (PHYS & PS)
  int numRejected;           // number regected (numSeen - numAccepted)

};

struct scaLxRecord
{
  char type[4];              // DB::
  int sz;                    // sizeof(scaLxCounts)
  unsigned int tm;                   // unixtime
  scaLxCounts rec;
};
#endif
