#ifndef ST_NNPDF_PYTHIA_EVENT_MAKER_H
#define ST_NNPDF_PYTHIA_EVENT_MAKER_H

class TFile;
class TTree;
class StPythiaEvent;

#include "StMaker.h"
#include "TString.h"

class StNNPDFPythiaEventMaker : public StMaker {
public:
  StNNPDFPythiaEventMaker(const char* name = "PythiaEvent")
    : StMaker(name)
    , mFileName("pythia.root")
    , mFile(0)
    , mTree(0)
    , mPythiaEvent(0)
  {
  }

  void Clear(Option_t* option = "");
  int  Init();
  int  Make();
  int  Finish();
  void SetPythiaFile(const char* filename) { mFileName = filename; }

private:
  TString mFileName;
  TFile* mFile;
  TTree* mTree;
  StPythiaEvent* mPythiaEvent;

  ClassDef(StNNPDFPythiaEventMaker,0);
};
#endif	// ST_PYTHIA_EVENT_MAKER_H
