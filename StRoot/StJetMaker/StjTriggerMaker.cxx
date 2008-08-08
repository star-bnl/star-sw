// $Id: StjTriggerMaker.cxx,v 1.3 2008/08/08 21:16:34 tai Exp $
#include "StjTriggerMaker.h"

#include "StjTrgPassCondition.h"

#include "StjTrgMuDst.h"

#include "StjTrgJPWriter.h"
#include "StjTrgHTWriter.h"
#include "StjTrgMBWriter.h"

#include "StjTrgMuDst.h"

#include "StjTrgSoftwareEmcTriggerMakerFactory.h"
#include "StjTrgSoftwareTriggerSimuMakerFactory.h"

#include <vector>

using namespace std;

ClassImp(StjTriggerMaker)
  

StjTriggerMaker::StjTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker, bool isMC)
  : StMaker(name)
  , _file(file)
  , _isMC(isMC)
  , _uDstMaker(uDstMaker)
  , _softTrgFactory(new StjTrgSoftwareEmcTriggerMakerFactory(emcTrigMaker))
{ }

StjTriggerMaker::StjTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StTriggerSimuMaker* simuTrig, bool isMC)
  : StMaker(name)
  , _file(file)
  , _isMC(isMC)
  , _uDstMaker(uDstMaker)
  , _softTrgFactory(new StjTrgSoftwareTriggerSimuMakerFactory(simuTrig))
{ }

void StjTriggerMaker::addTrgMB(const char *treeName, const char* treeTitle, int trgId)
{
  StjTrgMuDst* trg = new StjTrgMuDst(trgId, _uDstMaker, _softTrgFactory->create());
  StjTrgPassCondition* fillCondition;
  StjTrgPassCondition* passCondition;
  if(_isMC) {
    fillCondition = new StjTrgPassConditionSoftOnly(trg);
    passCondition = new StjTrgPassConditionSoftOnly(trg);
  } else {
    fillCondition = new StjTrgPassConditionHardOnly(trg);
    passCondition = new StjTrgPassConditionHardOnly(trg);
  }
  _writerList.push_back(new StjTrgMBWriter(treeTitle, treeTitle, _file, trg, fillCondition, passCondition));
}

void StjTriggerMaker::addTrgHT(const char *treeName, const char* treeTitle, int trgId)
{
  StjTrgMuDst* trg = new StjTrgMuDst(trgId, _uDstMaker, _softTrgFactory->create());
  StjTrgPassCondition* fillCondition;
  StjTrgPassCondition* passCondition;
  if(_isMC) {
    fillCondition = new StjTrgPassConditionSoftOnly(trg);
    passCondition = new StjTrgPassConditionSoftOnly(trg);
  } else {
    fillCondition = new StjTrgPassConditionHardOrSoft(trg);
    passCondition = new StjTrgPassConditionHardAndSoft(trg);
  }
  _writerList.push_back(new StjTrgHTWriter(treeTitle, treeTitle, _file, trg, fillCondition, passCondition));
}

void StjTriggerMaker::addTrgJP(const char *treeName, const char* treeTitle, int trgId)
{
  StjTrgMuDst* trg = new StjTrgMuDst(trgId, _uDstMaker, _softTrgFactory->create());
  StjTrgPassCondition* fillCondition;
  StjTrgPassCondition* passCondition;
  if(_isMC) {
    fillCondition = new StjTrgPassConditionSoftOnly(trg);
    passCondition = new StjTrgPassConditionSoftOnly(trg);
  } else {
    fillCondition = new StjTrgPassConditionHardOrSoft(trg);
    passCondition = new StjTrgPassConditionHardAndSoft(trg);
  }
  _writerList.push_back(new StjTrgJPWriter(treeTitle, treeTitle, _file, trg, fillCondition, passCondition));
}

Int_t StjTriggerMaker::Init()
{
  for(WriterList::const_iterator it = _writerList.begin(); it != _writerList.end(); ++it) {
    (*it)->Init();
  }
    
  return kStOk;
}

Int_t StjTriggerMaker::Make()
{
  for(WriterList::const_iterator it = _writerList.begin(); it != _writerList.end(); ++it) {
    (*it)->Make();
  }

  return kStOk;
}

Int_t StjTriggerMaker::Finish()
{
  for(WriterList::const_iterator it = _writerList.begin(); it != _writerList.end(); ++it) {
    (*it)->Finish();
  }

  return kStOk;
}
