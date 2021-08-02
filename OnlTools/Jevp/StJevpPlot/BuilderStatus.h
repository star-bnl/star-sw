#ifndef BuilderStatus_h
#define BuilderStatus_h

#include <Rtypes.h>
#include <TROOT.h>
#include <iostream>
#include <iomanip>

using namespace std;


class BuilderStatus : public TObject {
 public:
  // Set by the builder...
  char *name;
  int run;
  char *status;
  int lastEventTime;
  int events;
  
  //int detectorsNeeded;

  // These are used only by the server...
  //unsigned long long int sockid;  
  //int lastTransaction;
  //int official;

 public:

  BuilderStatus();
  void setName(const char *);
  void setStatus(const char *);
  ~BuilderStatus();

  bool running() {
    if(strcmp(status, "running") == 0) return true;
    return false;
  }

 ClassDef(BuilderStatus,1) ;
};

#endif


