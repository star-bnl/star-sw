#ifndef DAQMONDB_HH
#define DAQMONDB_HH

#include "rtsMonitor.h"

struct daqMonDB
{
  rtsMonRequired mon;
  char destination[20];
  int dest_port;
  int last_send;
  int backlog_size;
};


#endif
