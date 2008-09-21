// -*- mode: c++;-*-
// $Id: StjTrgHTWriter.h,v 1.4 2008/09/21 19:11:40 tai Exp $
#ifndef STJTRGHTWRITER_H
#define STJTRGHTWRITER_H

#include "StjTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StjTrg;

class StjTrgHTWriter : public StjTrgWriter {

public:

  StjTrgHTWriter(const char *treeName, const char* treeTitle,
		 TDirectory* file, StjTrg* trg,
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
  Int_t    _towerDsmAdc[4800];
  UInt_t   _towerAdc[4800];
  Double_t _towerEnergy[4800];
  Double_t _towerEt[4800];

  StjTrg* _trg;
  ClassDef(StjTrgHTWriter, 1)

};

#endif // STJTRGHTWRITER_H
