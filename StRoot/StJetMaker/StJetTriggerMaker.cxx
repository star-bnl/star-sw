// $Id: StJetTriggerMaker.cxx,v 1.2 2008/07/23 02:34:05 tai Exp $
#include "StJetTriggerMaker.h"

#include "StJetTrg.h"

#include "StJetTrgJPWriter.h"
#include "StJetTrgHTWriter.h"
#include "StJetTrgMBWriter.h"

#include "StJetTrg.h"

#include <vector>

using namespace std;

ClassImp(StJetTriggerMaker)
  

StJetTriggerMaker::StJetTriggerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker)
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

Int_t StJetTriggerMaker::Init()
{
  StJetTrg* trg = new StJetTrg(_uDstMaker, new StJetTrgSoftware(_emcTrigMaker));


  _minbWriter = new StJetTrgMBWriter("trgMINB", "trgMINB", 96011, _file, trg);

  _bht1Writer = new StJetTrgHTWriter("trgBHT1", "trgBHT1", 96201, _file, trg);

  _bht2Writer = new StJetTrgHTWriter("trgBHT2", "trgBHT2", 96211, _file, trg);

  _bjp1Writer = new StJetTrgJPWriter("trgBJP1", "trgBJP1", 96221, _file, trg);

  _bjp2Writer = new StJetTrgJPWriter("trgBJP2", "trgBJP2", 96233, _file, trg);


  _minbWriter->Init();
  _bht1Writer->Init();
  _bht2Writer->Init();
  _bjp1Writer->Init();
  _bjp2Writer->Init();

  return kStOk;
}

Int_t StJetTriggerMaker::Make()
{
  _minbWriter->Make();
  _bht1Writer->Make();
  _bht2Writer->Make();
  _bjp1Writer->Make();
  _bjp2Writer->Make();

  return kStOk;
}

Int_t StJetTriggerMaker::Finish()
{
  _minbWriter->Finish();
  _bht1Writer->Finish();
  _bht2Writer->Finish();
  _bjp1Writer->Finish();
  _bjp2Writer->Finish();

  return kStOk;
}
