// -*- mode: c++;-*-
// $Id: StjTriggerMaker.h,v 1.3 2008/08/02 22:43:07 tai Exp $
#ifndef STJTRIGGERMAKER_H
#define STJTRIGGERMAKER_H

#include "StMaker.h"
#include <Rtypes.h>

#include <vector>

class TDirectory;
class TTree;

class StjTrgWriter;

class StMuDstMaker;
class StEmcTriggerMaker;
class StTriggerSimuMaker;

class StjTrgSoftwareFactory;

class StjTrg;

class StjTriggerMaker : public StMaker {

public:

  StjTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker, bool isMC = false);
  StjTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StTriggerSimuMaker* simuTrig, bool isMC = false);
  virtual ~StjTriggerMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();

  void addTrgMB(const char *treeName, const char* treeTitle, int trgId);
  void addTrgHT(const char *treeName, const char* treeTitle, int trgId);
  void addTrgJP(const char *treeName, const char* treeTitle, int trgId);

    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjTriggerMaker.h,v 1.3 2008/08/02 22:43:07 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:


  TDirectory* _file;

  bool _isMC;

  StMuDstMaker* _uDstMaker;
  StjTrgSoftwareFactory* _softTrgFactory;

  typedef std::vector<StjTrgWriter*> WriterList;
  WriterList _writerList;

  ClassDef(StjTriggerMaker, 0)

};

#endif // STJTRIGGERMAKER_H
