#ifndef MAPS_DIGCLUSTER_H
#define MAPS_DIGCLUSTER_H

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
class DIGAction;
class DIGEvent;
class DIGMAPS;
//==============================================================================
class DIGCluster : public TObject {
 public:
  DIGCluster();
  DIGCluster(DIGCluster& adigCluster);
  virtual ~DIGCluster();
  void    Clear(const Option_t * /*option*/ = "");
  void PrintInfo();

  Int_t GetNpixels(){return fNpixels;}
  std::vector<Int_t> GetPixelMap(){return fPixelMap;}
  std::vector<Int_t> GetDigitalCharge(){return fDigitalChargeMap;}

  void AddPixel(Int_t DigitalCharge, Int_t PixelNumber);
  Int_t GetTotalCharge();
  Int_t Get1stCrownCharge(DIGPlane *myDIGPlane);
  Int_t Get2ndCrownCharge(DIGPlane *myDIGPlane);
  Int_t Get4NeigboursCharge(DIGPlane *myDIGPlane);

  Int_t GetMultiplicity(Int_t Threshold);
  Double_t GetXposition_CoG(){ return Xposition_CoG;}
  Double_t GetYposition_CoG(){ return Yposition_CoG;}
  void SetXposition_CoG(Double_t aXposition_CoG){Xposition_CoG = aXposition_CoG;}
  void SetYposition_CoG(Double_t aYposition_CoG){Yposition_CoG = aYposition_CoG ;}
  void Compute_CoG(DIGPlane *myDIGPlane);
  void Compute_SeedPixel(DIGPlane *myDIGPlane); // compute the seed pixel.
  Int_t GetSeedPixelIndex(); //return the seed pixel index in the pixel map.
  void SetSeedPixelIndex(Int_t Index);
  void GetXYPixelNumber(Int_t &Xpix, Int_t &Ypix, DIGPlane *myDIGPlane, Int_t PixelNumber);
  Bool_t IsPixelInThePlane(Int_t Xpix, Int_t Ypix, DIGPlane *myDIGPlane);

  std::vector<Int_t> Get4NeigboursPixelsIndex(DIGPlane *myDIGPlane);
  std::vector<Int_t> Get1stCrownPixelsIndex(DIGPlane *myDIGPlane);
  std::vector<Int_t> Get2ndCrownPixelsIndex(DIGPlane *myDIGPlane);


 protected:
  //
  Int_t fNpixels;
  vector< Int_t > fPixelMap;
  vector< Int_t > fDigitalChargeMap;
  Double_t Xposition_CoG;
  Double_t Yposition_CoG;
  Int_t fSeedPixelIndex;

  ClassDef(DIGCluster,1);
};



//==============================================================================

#endif
