#ifndef BuilderStatus_h
#define BuilderStatus_h

#include <Rtypes.h>
#include <TROOT.h>
#include <iostream>
#include <iomanip>
using namespace std;


class BuilderStatus : public TObject {
 public:
  char *name;
  int run;
  char *status;
  int lastEventTime;
  int events;
  
  int detectorsNeeded;

 public:

  BuilderStatus();
  void setName(char *);
  void setStatus(char *);
  ~BuilderStatus();

  bool running() {
    if(strcmp(status, "running") == 0) return true;
  }

 ClassDef(BuilderStatus,1) ;
};

#endif


