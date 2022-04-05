#ifndef _DAQSUMEVTCNTS_H_
#define _DAQSUMEVTCNTS_H_


struct daqsumEvtCnts {	
  int run;
  int nevts;    // number of distinct events taken
  int nwevts;   // number of events written to a file 
                // (multiple copies count multiple times)
  int evb;
  unsigned int runStart;
  unsigned int runStop;  
};

#endif
