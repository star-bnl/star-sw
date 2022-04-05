#ifndef MAPS_ADC_H
#define MAPS_ADC_H

#include <TNamed.h>
#include <TList.h>
#include <TGraph.h>
#include "Riostream.h"
#include "vector"

// ROOT classes
#include "TString.h"
#include "TObject.h"
#include "TVector.h"
#include "TFile.h"
#include "TSystem.h"
#include "TRandom.h"
#include "TH1.h"
#include "TH2.h"
#include "TObjArray.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"
using namespace std;
//class DIGADC;

class DIGInitialize;
class DIGPlane;
class DIGADC;
class DIGBeam;
class DIGTransport;
class DIGParticle;
class DIGAction;
class DIGEvent;
class DIGMAPS;
//==============================================================================
class DIGADC : public TObject {
 public:
  DIGADC();
  //  DIGPlane(char *name,char *title);
  virtual ~DIGADC();

  //  void AddLayer(char *name, Float_t radius, Float_t radL, Float_t phiRes=99999, Float_t zRes=99999, Float_t integrationTime=-1.);


  void SetNbits(Int_t Nbits);
  void SetNThresholds(Int_t NThresholds);
  void SetADC_linear(Bool_t ADC_linear);
  void SetLSB(Float_t LSB);
  void SetElectron_Conversion(Float_t Electron_Conversion);
  void SetADC_thresholds(Float_t ADC_thresholds[], Int_t NThresholds);

  void PrintInfo();
  Int_t GetNbits(){return fNbits;}
  Int_t GetNThresholds(){return fNThresholds;}
  Bool_t GetADC_linear(){return fADC_linear;}
  Float_t GetLSB(){return fLSB;}
  Float_t GetElectron_Conversion(){return fElectron_Conversion;}
  Float_t *GetADC_thresholds(){return fADC_thresholds;}
  //  Float_t Get(){return ;}


 protected:
  enum       {adcnum = 4096}; //12bits maximum
  Int_t fNbits;
  Int_t fNThresholds; // actually (2^Nbits)-1
  Bool_t fADC_linear;
  Float_t fLSB;
  Float_t fElectron_Conversion;
  Float_t fADC_thresholds[adcnum]; //in S/N units.

 

  ClassDef(DIGADC,1);
};

//==============================================================================

#endif
