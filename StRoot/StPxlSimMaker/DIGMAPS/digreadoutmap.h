#ifndef MAPS_DIGREADOUTMAP_H
#define MAPS_DIGREADOUTMAP_H

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
class DIGReadoutmap : public TObject {
 public:
  DIGReadoutmap();
  DIGReadoutmap(Int_t Npixels);
  DIGReadoutmap(DIGReadoutmap& adigreadoutmap);
  virtual ~DIGReadoutmap();
  void    Clear(const Option_t * /*option*/ = "");
  void PrintInfo();
  void PrintOuput(Int_t Nx ,Int_t Ny);

  Int_t GetNpixels(){return fNpixels;}
  std::vector<Int_t> GetPixelMap(){return fPixelMap;}
  std::vector<Float_t> GetAnalogCharge(){return fAnalogChargeMap;}
  std::vector<Int_t> GetDigitalCharge(){return fDigitalChargeMap;}

  void SetNpixels(Int_t Npixels){fNpixels = Npixels;}
  void AddPixel(Float_t AnalogCharge, Int_t PixelNumber);
  void UpdatePixel(Float_t AnalogCharge, Int_t PixelNumber);
  void AnalogToDigitalconversion(DIGADC *myDIGADC,  DIGPlane *myDIGPlane );

 protected:

  Int_t fNpixels;
  vector< Int_t > fPixelMap;
  vector< Float_t > fAnalogChargeMap;
  vector< Int_t > fDigitalChargeMap;
  

  ClassDef(DIGReadoutmap,1);
};



//==============================================================================

#endif
