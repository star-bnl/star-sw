// -*- mode: c++;-*-
// $Id: StJetTriggerMaker.h,v 1.3 2008/07/24 02:14:51 tai Exp $
#ifndef STJETTRIGGERMAKER_HH
#define STJETTRIGGERMAKER_HH

#include "StMaker.h"
#include <Rtypes.h>

#include <vector>

class TDirectory;
class TTree;

class StJetTrgWriter;

class StMuDstMaker;
class StEmcTriggerMaker;
class StTriggerSimuMaker;

class StJetTrgSoftwareFactory;

class StJetTrg;

class StJetTriggerMaker : public StMaker {

public:

  StJetTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker, bool isMC = false);
  StJetTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StTriggerSimuMaker* simuTrig, bool isMC = false);
  virtual ~StJetTriggerMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();

  void addTrgMB(const char *treeName, const char* treeTitle, int trgId);
  void addTrgHT(const char *treeName, const char* treeTitle, int trgId);
  void addTrgJP(const char *treeName, const char* treeTitle, int trgId);

    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetTriggerMaker.h,v 1.3 2008/07/24 02:14:51 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:


  TDirectory* _file;

  bool _isMC;

  StMuDstMaker* _uDstMaker;
  StJetTrgSoftwareFactory* _softTrgFactory;

  typedef std::vector<StJetTrgWriter*> WriterList;
  WriterList _writerList;

  ClassDef(StJetTriggerMaker, 0)

};

#endif // STJETTRIGGERMAKER_HH
