// -*- mode: c++;-*-
// $Id: StjTrgHTWriter.h,v 1.4 2008/08/08 22:53:18 tai Exp $
#ifndef STJTRGHTWRITER_H
#define STJTRGHTWRITER_H

#include "StjTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StjTrgMuDst;

class StjTrgHTWriter : public StjTrgWriter {

public:

  StjTrgHTWriter(const char *treeName, const char* treeTitle,
		 TDirectory* file, StjTrgMuDst* trg,
		 StjTrgPassCondition* fillCondition)
    : StjTrgWriter(treeName, treeTitle, file, trg, fillCondition)
    , _trg(trg)
  { }
  virtual ~StjTrgHTWriter() { }

private:

  virtual void createBranch_trgSpecific(TTree* tree);
  virtual void fillBranch_trgSpecific();

  Int_t    _nTowers;
  Int_t    _towerId[4800];

  StjTrgMuDst* _trg;
};

#endif // STJTRGHTWRITER_H
