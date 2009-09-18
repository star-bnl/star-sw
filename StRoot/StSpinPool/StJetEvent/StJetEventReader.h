// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 15 September 2009
//

#ifndef ST_JET_EVENT_READER_H
#define ST_JET_EVENT_READER_H

// C++ STL
#include <vector>
using std::vector;

// ROOT
class TChain;

// STAR
class StJetEvent;
class StJetEventAnalyzer;
class StJetSkimEvent;

// ROOT
#include "TObject.h"

class StJetEventReader : public TObject {
public:
  StJetEventReader(const char* jetfile, const char* skimfile);
  ~StJetEventReader();

  // Getters
  StJetEvent* getJetEvent(const char* branchname) const;
  StJetSkimEvent* getSkimEvent(const char* branchname) const;

  // Setters
  void addJetBranch(const char* branchname);
  void addSkimBranch(const char* branchname);
  void attach(StJetEventAnalyzer* a) { mAnalyzers.push_back(a); }

  // Actions
  bool next() const;
  bool operator()() const { return next(); }
  void eventLoop(int nentries);

private:
  TChain* mJetChain;
  TChain* mSkimChain;
  mutable int mEntry;
  vector<StJetEventAnalyzer*> mAnalyzers;

  ClassDef(StJetEventReader,1);
};

#endif // ST_JET_EVENT_READER_H
