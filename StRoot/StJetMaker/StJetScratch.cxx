// $Id: StJetScratch.cxx,v 1.1 2008/07/11 23:32:20 tai Exp $
#include "StJetScratch.h"

#include "StJetTrgJPWriter.h"
#include "StJetTrgHTWriter.h"
#include "StJetTrgMBWriter.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StEmcTriggerMaker/StEmcTriggerMaker.h>

#include <TFile.h>
#include <TTree.h>

#include <map>
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

ClassImp(StJetScratch)
  

StJetScratch::StJetScratch(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker)
  : StMaker(name)
  , _file(file)
  , _uDstMaker(uDstMaker)
  , _emcTrigMaker(emcTrigMaker)
  , _minbWriter(0)
  , _bht1Writer(0)
  , _bht2Writer(0)
  , _bjp1Writer(0)
  , _bjp2Writer(0)
{ }

Int_t StJetScratch::Init()
{
  _minbWriter = new StJetTrgMBWriter("trgMINB", "trgMINB", 96011, _file, _uDstMaker);

  _bht1Writer = new StJetTrgHTWriter("trgBHT1", "trgBHT1", 96201, _file, _uDstMaker, _emcTrigMaker);

  _bht2Writer = new StJetTrgHTWriter("trgBHT2", "trgBHT2", 96211, _file, _uDstMaker, _emcTrigMaker);

  _bjp1Writer = new StJetTrgJPWriter("trgBJP1", "trgBJP1", 96221, _file, _uDstMaker, _emcTrigMaker);

  _bjp2Writer = new StJetTrgJPWriter("trgBJP2", "trgBJP2", 96233, _file, _uDstMaker, _emcTrigMaker);


  _minbWriter->Init();
  _bht1Writer->Init();
  _bht2Writer->Init();
  _bjp1Writer->Init();
  _bjp2Writer->Init();

  return kStOk;
}

Int_t StJetScratch::Make()
{
  _minbWriter->Make();
  _bht1Writer->Make();
  _bht2Writer->Make();
  _bjp1Writer->Make();
  _bjp2Writer->Make();

  return kStOk;
}

Int_t StJetScratch::Finish()
{
  _minbWriter->Finish();
  _bht1Writer->Finish();
  _bht2Writer->Finish();
  _bjp1Writer->Finish();
  _bjp2Writer->Finish();

  return kStOk;
}
