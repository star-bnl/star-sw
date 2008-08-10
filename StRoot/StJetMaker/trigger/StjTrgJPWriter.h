// -*- mode: c++;-*-
// $Id: StjTrgJPWriter.h,v 1.5 2008/08/10 23:04:57 tai Exp $
#ifndef STJTRGJPWRITER_H
#define STJTRGJPWRITER_H

#include "StjTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StjTrg;

class StjTrgJPWriter : public StjTrgWriter {

public:

  StjTrgJPWriter(const char *treeName, const char* treeTitle,
		 TDirectory* file, StjTrg* trg,
		 StjTrgPassCondition* fillCondition)
    : StjTrgWriter(treeName, treeTitle, file, trg, fillCondition)
    , _trg(trg)
  { }
  virtual ~StjTrgJPWriter() { }

private:

  virtual void createBranch_trgSpecific(TTree* tree);
  virtual void fillBranch_trgSpecific();

  Int_t    _nJetPatches;
  Int_t    _jetPatchId[12];

  StjTrg* _trg;
};

#endif // STJTRGJPWRITER_H
