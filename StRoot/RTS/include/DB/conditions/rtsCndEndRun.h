#ifndef _RTSCNDENDRUN_H_
#define _RTSCNDENDRUN_H_

struct rtsCndEndRun
{
  ///////////
  int idx_rn;
  int idx_stoptime;
  ///////////

  int status;
  int n_events;
  int n_files;
  int junk;
  int fin;
};
  
#endif
