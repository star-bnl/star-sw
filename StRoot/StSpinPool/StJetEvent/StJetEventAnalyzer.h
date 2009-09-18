// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 16 September 2009
//

#ifndef ST_JET_EVENT_ANALYZER_H
#define ST_JET_EVENT_ANALYZER_H

// STAR
class StJetEventReader;

// ROOT
#include "TObject.h"

struct StJetEventAnalyzer : public TObject {
  virtual void analyze(const StJetEventReader*) = 0;

  ClassDef(StJetEventAnalyzer,1);
};

#endif // ST_JET_EVENT_ANALYZER_H
