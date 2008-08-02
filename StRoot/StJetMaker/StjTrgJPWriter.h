// -*- mode: c++;-*-
// $Id: StjTrgJPWriter.h,v 1.1 2008/08/02 04:06:46 tai Exp $
#ifndef STJETTRGJPWRITER_H
#define STJETTRGJPWRITER_H

#include "StjTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StJetTrg;

class StJetTrgJPWriter : public StJetTrgWriter {

public:

  StJetTrgJPWriter(const char *treeName, const char* treeTitle,
		   TDirectory* file, StJetTrg* trg,
		   StJetTrgPassCondition* fillCondition,
		   StJetTrgPassCondition* passCondition)
    : StJetTrgWriter(treeName, treeTitle, file, trg, fillCondition, passCondition)
    , _trg(trg)
  { }
  virtual ~StJetTrgJPWriter() { }

private:

  virtual void createBranch_trgSpecific(TTree* tree);
  virtual void fillBranch_trgSpecific();

  Int_t    _nJetPatches;
  Int_t    _jetPatchId[12];

  StJetTrg* _trg;
};

#endif // STJETTRGJPWRITER_H
