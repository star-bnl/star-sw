// -*- mode: c++;-*-
// $Id: StJetTrgHTWriter.h,v 1.6 2008/07/23 23:21:04 tai Exp $
#ifndef STJETTRGHTWRITER_H
#define STJETTRGHTWRITER_H

#include "StJetTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StJetTrg;

class StJetTrgHTWriter : public StJetTrgWriter {

public:

  StJetTrgHTWriter(const char *treeName, const char* treeTitle, int trgId, TDirectory* file, StJetTrg* trg)
    : StJetTrgWriter(treeName, treeTitle, trgId, file, trg)
    , _trgId(trgId)
    , _trg(trg)
  { }
  virtual ~StJetTrgHTWriter() { }

private:

  virtual void createBranch_trgSpecific(TTree* tree);
  virtual void fillBranch_trgSpecific();

  Int_t    _nTowers;
  Int_t    _towerId[4800];

  int _trgId;
  StJetTrg* _trg;
};

#endif // STJETTRGHTWRITER_H
