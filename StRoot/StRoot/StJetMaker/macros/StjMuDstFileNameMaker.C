#include "StMaker.h"


#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <TChain.h>
#include <TDirectory.h>

#include <libgen.h>

#include <string>
#include <cstring>

using namespace std;

class StjMuDstFileNameMaker : public StMaker {

public:

  StjMuDstFileNameMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  { }

  virtual ~StjMuDstFileNameMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjMuDstFileNameMaker.C,v 1.2 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;
  TTree* _tree;

  StMuDstMaker* _uDstMaker;

  Int_t _runNumber;
  Int_t _eventId;
  Double_t _vertexZ;
  Char_t _mudstName[256];

  string getMuDstFileName()
  {
    string filepath(_uDstMaker->chain()->GetFile()->GetName());
    char *chFilepath = new char[filepath.size() + 1];
    sprintf(chFilepath, "%s", filepath.c_str());
    string filename = basename(chFilepath);
    delete[] chFilepath;
    return filename;
  }

public:

  Int_t Init()
  {
    _file->cd();
    _tree = new TTree("mudstName", "mudstName");
    _tree->Branch("runNumber"  , &_runNumber  , "runNumber/I"      );
    _tree->Branch("eventId"    , &_eventId    , "eventId/I"        );
    _tree->Branch("vertexZ"    , &_vertexZ    , "vertexZ/D"   );    
    _tree->Branch("mudstName"  , &_mudstName  , "mudstName[256]/C" );
    return kStOk;
  }

  Int_t Make()
  {
    string finlename = getMuDstFileName();
    _runNumber = _uDstMaker->muDst()->event()->runId();
    _eventId = _uDstMaker->muDst()->event()->eventId();
    _vertexZ = _uDstMaker->muDst()->event()->primaryVertexPosition().z();

    strcpy(_mudstName, finlename.c_str());
    _tree->Fill();
    return kStOk;
  }

  Int_t Finish()
  {
    _tree->BuildIndex("runNumber", "eventId");
    return kStOk;
  }
  ClassDef(StjMuDstFileNameMaker, 0)
};
