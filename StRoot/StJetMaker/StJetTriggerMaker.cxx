// $Id: StJetTriggerMaker.cxx,v 1.3 2008/07/23 20:25:43 tai Exp $
#include "StJetTriggerMaker.h"

#include "StJetTrg.h"

#include "StJetTrgJPWriter.h"
#include "StJetTrgHTWriter.h"
#include "StJetTrgMBWriter.h"

#include "StJetTrg.h"

#include "StJetTrgSoftwareEmcTriggerMaker.h"
#include "StJetTrgSoftwareTriggerSimuMaker.h"

#include <vector>

using namespace std;

ClassImp(StJetTriggerMaker)
  

StJetTriggerMaker::StJetTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker)
  : StMaker(name)
  , _file(file)
  , _trg(new StJetTrg(uDstMaker, new StJetTrgSoftwareEmcTriggerMaker(emcTrigMaker)))
{ }

StJetTriggerMaker::StJetTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StTriggerSimuMaker* simuTrig)
  : StMaker(name)
  , _file(file)
  , _trg(new StJetTrg(uDstMaker, new StJetTrgSoftwareTriggerSimuMaker(simuTrig)))
{ }

void StJetTriggerMaker::addTrgMB(const char *treeName, const char* treeTitle, int trgId)
{
  _writerList.push_back(new StJetTrgMBWriter(treeTitle, treeTitle, trgId, _file, _trg));
}

void StJetTriggerMaker::addTrgHT(const char *treeName, const char* treeTitle, int trgId)
{
  _writerList.push_back(new StJetTrgHTWriter(treeTitle, treeTitle, trgId, _file, _trg));
}

void StJetTriggerMaker::addTrgJP(const char *treeName, const char* treeTitle, int trgId)
{
  _writerList.push_back(new StJetTrgJPWriter(treeTitle, treeTitle, trgId, _file, _trg));
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
