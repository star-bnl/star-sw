// $Id: StjTriggerMaker.cxx,v 1.1 2008/08/02 04:08:41 tai Exp $
#include "StjTriggerMaker.h"

#include "StjTrgPassCondition.h"

#include "StjTrg.h"

#include "StjTrgJPWriter.h"
#include "StjTrgHTWriter.h"
#include "StjTrgMBWriter.h"

#include "StjTrg.h"

#include "StjTrgSoftwareEmcTriggerMakerFactory.h"
#include "StjTrgSoftwareTriggerSimuMakerFactory.h"

#include <vector>

using namespace std;

ClassImp(StJetTriggerMaker)
  

StJetTriggerMaker::StJetTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker, bool isMC)
  : StMaker(name)
  , _file(file)
  , _isMC(isMC)
  , _uDstMaker(uDstMaker)
  , _softTrgFactory(new StJetTrgSoftwareEmcTriggerMakerFactory(emcTrigMaker))
{ }

StJetTriggerMaker::StJetTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StTriggerSimuMaker* simuTrig, bool isMC)
  : StMaker(name)
  , _file(file)
  , _isMC(isMC)
  , _uDstMaker(uDstMaker)
  , _softTrgFactory(new StJetTrgSoftwareTriggerSimuMakerFactory(simuTrig))
{ }

void StJetTriggerMaker::addTrgMB(const char *treeName, const char* treeTitle, int trgId)
{
  StJetTrg* trg = new StJetTrg(trgId, _uDstMaker, _softTrgFactory->create());
  StJetTrgPassCondition* fillCondition;
  StJetTrgPassCondition* passCondition;
  if(_isMC) {
    fillCondition = new StJetTrgPassConditionSoftOnly(trg);
    passCondition = new StJetTrgPassConditionSoftOnly(trg);
  } else {
    fillCondition = new StJetTrgPassConditionHardOnly(trg);
    passCondition = new StJetTrgPassConditionHardOnly(trg);
  }
  _writerList.push_back(new StJetTrgMBWriter(treeTitle, treeTitle, _file, trg, fillCondition, passCondition));
}

void StJetTriggerMaker::addTrgHT(const char *treeName, const char* treeTitle, int trgId)
{
  StJetTrg* trg = new StJetTrg(trgId, _uDstMaker, _softTrgFactory->create());
  StJetTrgPassCondition* fillCondition;
  StJetTrgPassCondition* passCondition;
  if(_isMC) {
    fillCondition = new StJetTrgPassConditionSoftOnly(trg);
    passCondition = new StJetTrgPassConditionSoftOnly(trg);
  } else {
    fillCondition = new StJetTrgPassConditionHardOrSoft(trg);
    passCondition = new StJetTrgPassConditionHardAndSoft(trg);
  }
  _writerList.push_back(new StJetTrgHTWriter(treeTitle, treeTitle, _file, trg, fillCondition, passCondition));
}

void StJetTriggerMaker::addTrgJP(const char *treeName, const char* treeTitle, int trgId)
{
  StJetTrg* trg = new StJetTrg(trgId, _uDstMaker, _softTrgFactory->create());
  StJetTrgPassCondition* fillCondition;
  StJetTrgPassCondition* passCondition;
  if(_isMC) {
    fillCondition = new StJetTrgPassConditionSoftOnly(trg);
    passCondition = new StJetTrgPassConditionSoftOnly(trg);
  } else {
    fillCondition = new StJetTrgPassConditionHardOrSoft(trg);
    passCondition = new StJetTrgPassConditionHardAndSoft(trg);
  }
  _writerList.push_back(new StJetTrgJPWriter(treeTitle, treeTitle, _file, trg, fillCondition, passCondition));
}

Int_t StJetTriggerMaker::Init()
{
  for(WriterList::const_iterator it = _writerList.begin(); it != _writerList.end(); ++it) {
    (*it)->Init();
  }
    
  return kStOk;
}

Int_t StJetTriggerMaker::Make()
{
  for(WriterList::const_iterator it = _writerList.begin(); it != _writerList.end(); ++it) {
    (*it)->Make();
  }

  return kStOk;
}

Int_t StJetTriggerMaker::Finish()
{
  for(WriterList::const_iterator it = _writerList.begin(); it != _writerList.end(); ++it) {
    (*it)->Finish();
  }

  return kStOk;
}
