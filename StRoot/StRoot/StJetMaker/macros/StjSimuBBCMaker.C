#include "StMaker.h"


#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StTriggerUtilities/StTriggerSimuMaker.h>
#include <StTriggerUtilities/StTriggerSimuResult.h>
#include <StTriggerUtilities/Bemc/StBemcTriggerSimu.h>

#include <TChain.h>
#include <TDirectory.h>

#include <libgen.h>

#include <string>
#include <cstring>

using namespace std;

class StjSimuBBCMaker : public StMaker {

public:

  StjSimuBBCMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker, int trgID)
    : StMaker(name), _file(file), _uDstMaker(uDstMaker), _trgID(trgID)
  { }

  virtual ~StjSimuBBCMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjSimuBBCMaker.C,v 1.3 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;
  TTree* _tree;

  int _trgID;

  StMuDstMaker* _uDstMaker;

  Int_t _runNumber;
  Int_t _eventId;
  Int_t _bbc;
  Double_t _vertexZ;

public:

  Int_t Init()
  {
    _file->cd();
    _tree = new TTree("simuBBC", "simuBBC");
    _tree->Branch("runNumber"  , &_runNumber  , "runNumber/I"  );
    _tree->Branch("eventId"    , &_eventId    , "eventId/I"    );
    _tree->Branch("bbc"        , &_bbc        , "bbc/I"        );
    _tree->Branch("vertexZ"    , &_vertexZ    , "vertexZ/D"    );    
    return kStOk;
  }

  Int_t Make()
  {
    _runNumber = _uDstMaker->muDst()->event()->runId();
    _eventId = _uDstMaker->muDst()->event()->eventId();
    _vertexZ = _uDstMaker->muDst()->event()->primaryVertexPosition().z();

    StTriggerSimuMaker* trigSimu = dynamic_cast<StTriggerSimuMaker*>(GetMaker("StarTrigSimu"));
    StTriggerSimuResult trigResult = trigSimu->detailedResult(_trgID);

    _bbc = trigResult.bbcDecision();

    _tree->Fill();
    return kStOk;
  }

  Int_t Finish()
  {
    _tree->BuildIndex("runNumber", "eventId");
    return kStOk;
  }
  ClassDef(StjSimuBBCMaker, 0)
};
