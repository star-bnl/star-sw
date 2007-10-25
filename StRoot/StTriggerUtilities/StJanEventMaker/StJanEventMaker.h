// -*- mode: c++ -*-
//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// Mar 1, 2006
//

#ifndef StJanEventMaker_h
#define StJanEventMaker_h

// C++ STL
#include <fstream>
using namespace std;

// STAR
#include "StMaker.h"

class StEvent;
class StEmcDecoder;
class JanEvent;

class StJanEventMaker : public StMaker {
public:
  StJanEventMaker(const string& name = "StJanEventMaker") : StMaker(name.c_str()) {}
  ~StJanEventMaker() {}

 
  Int_t Init();
  Int_t InitRun(Int_t runNumber);
  Int_t Make();
  Int_t Finish();

private:
  Char_t* triggerData(StEvent* event);
  void fillJanEvent(Char_t* trgData, UShort_t* bemcData, UShort_t* eemcData, JanEvent& event);

  ofstream mFile;
  Int_t mEventCounter;

  ClassDef(StJanEventMaker, 0)
};

#endif
