// -*- mode: c++;-*-
// $Id: StjTrgHTWriter.h,v 1.1 2008/08/02 04:06:34 tai Exp $
#ifndef STJETTRGHTWRITER_H
#define STJETTRGHTWRITER_H

#include "StjTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StJetTrg;

class StJetTrgHTWriter : public StJetTrgWriter {

public:

  StJetTrgHTWriter(const char *treeName, const char* treeTitle,
		   TDirectory* file, StJetTrg* trg,
		   StJetTrgPassCondition* fillCondition,
		   StJetTrgPassCondition* passCondition)
    : StJetTrgWriter(treeName, treeTitle, file, trg, fillCondition, passCondition)
    , _trg(trg)
  { }
  virtual ~StJetTrgHTWriter() { }

private:

  virtual void createBranch_trgSpecific(TTree* tree);
  virtual void fillBranch_trgSpecific();

  Int_t    _nTowers;
  Int_t    _towerId[4800];

  StJetTrg* _trg;
};

#endif // STJETTRGHTWRITER_H
