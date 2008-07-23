// -*- mode: c++;-*-
// $Id: StJetTrgJPWriter.h,v 1.6 2008/07/23 23:21:04 tai Exp $
#ifndef STJETTRGJPWRITER_H
#define STJETTRGJPWRITER_H

#include "StJetTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StJetTrg;

class StJetTrgJPWriter : public StJetTrgWriter {

public:

  StJetTrgJPWriter(const char *treeName, const char* treeTitle, int trgId, TDirectory* file, StJetTrg* trg)
    : StJetTrgWriter(treeName, treeTitle, trgId, file, trg)
    , _trgId(trgId)
    , _trg(trg)
  { }
  virtual ~StJetTrgJPWriter() { }

private:

  virtual void createBranch_trgSpecific(TTree* tree);
  virtual void fillBranch_trgSpecific();

  Int_t    _nJetPatches;
  Int_t    _jetPatchId[12];

  int _trgId;
  StJetTrg* _trg;
};

#endif // STJETTRGJPWRITER_H
