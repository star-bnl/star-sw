#ifndef _EVPMONITOR_H_
#define _EVPMONITOR_H_

// This is a root command line monitor program for the evp
// server....

#include "Jevp/StJevpServer/EvpConstants.h"
#include "TObject.h"

class EvpMonitor : public TObject 
{
 private:
  EvpMonitor() {
    server = (char *)"evp.starp.bnl.gov";
    serverport = JEVP_PORT;
  }  

 public:

  // Argument values
  char *server;
  int serverport;

  char serverCommand[1000];

  int parseArgs(int argc, char *argv[]);

  virtual ~EvpMonitor(){}
  static int _main(int argc, char **argv );
  static int main(char *args);
  ClassDef(EvpMonitor,0);
};

extern EvpMonitor *evpMonitor;

#endif
