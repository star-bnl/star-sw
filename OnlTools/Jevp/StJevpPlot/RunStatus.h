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

  int firstEvtTime;
  int firstEvtNumber;
  int lastEvtTime;
  int lastEvtNumber;
  int nEvts;

  int timeOfLastChange;

  RunStatus();
  ~RunStatus();
  
  void setStatus(const char *s);

  void dump();
 
  static int getNumericStatus(char *str);

  int running();
  void addEvent(int seq, int tm);

  ClassDef(RunStatus,1) ;
};

#endif


