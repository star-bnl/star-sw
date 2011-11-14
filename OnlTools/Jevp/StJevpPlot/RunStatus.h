#ifndef RunStatus_h
#define RunStatus_h

#include <Rtypes.h>
#include <TROOT.h>
#include <iostream>
#include <iomanip>

using namespace std;


class RunStatus : public TObject {
 public:
  int run;
  char *status;
  int timeOfLastChange;

  RunStatus();
  ~RunStatus();
  
  void setStatus(const char *s);

  void dump();
 
  static int getNumericStatus(char *str);

  int running();

  ClassDef(RunStatus,1) ;
};

#endif


