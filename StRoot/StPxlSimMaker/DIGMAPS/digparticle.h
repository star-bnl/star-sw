#ifndef MAPS_DIGPARTICLE_H
#define MAPS_DIGPARTICLE_H

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
extern Int_t GlobalSeed ;

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
class DIGParticle : public TObject {
 public:
  DIGParticle();
  DIGParticle(Float_t EntryX, Float_t EntryY, Float_t EntryZ,
	      Float_t ExitX, Float_t ExitY, Float_t ExitZ, Float_t Energy_deposited);
  DIGParticle(DIGParticle& adigparticle);
  virtual ~DIGParticle();
  void    Clear(const Option_t * /*option*/ = "");
  Float_t GetEntryX(){return fEntryX;}
  Float_t GetEntryY(){return fEntryY;}
  Float_t GetEntryZ(){return fEntryZ;}
  Float_t GetExitX(){return fExitX;}
  Float_t GetExitY(){return fExitY;}
  Float_t GetExitZ(){return fExitZ;}
  Float_t GetEnergy_deposited(){return fEnergy_deposited;}
  Float_t GetTotalAnalogCharge();
  Int_t GetTotalDigitalCharge();

  Int_t GetNSegment(){return fNSegment;}
  std::vector<Float_t> GetSegmentX(){return fSegmentX;}
  std::vector<Float_t> GetSegmentY(){return fSegmentY;}
  std::vector<Float_t> GetSegmentZ(){return fSegmentZ;}
  std::vector<Float_t> GetSegmentCharge(){return fSegmentCharge;}
  Int_t GetNpixels(){return fNpixels;}
  std::vector<Int_t> GetPixelMap(){return fPixelMap;}
  std::vector<Float_t> GetAnalogCharge(){return fAnalogChargeMap;}
  std::vector<Int_t> GetDigitalCharge(){return fDigitalChargeMap;}

  void SetNSegment(Int_t NSegment){fNSegment=NSegment;}  
  void SetEntryX(Float_t EntryX){fEntryX=EntryX;}
  void SetEntryY(Float_t EntryY){fEntryY=EntryY;}
  void SetEntryZ(Float_t EntryZ){fEntryY=EntryZ;}
  void SetExitX(Float_t ExitX){fExitX=ExitX;}
  void SetExitY(Float_t ExitY){fExitY=ExitY;}
  void SetExitZ(Float_t ExitZ){fExitZ=ExitZ;}
  void SetEnergy_deposited(Float_t Energy_deposited){fEnergy_deposited=Energy_deposited;}
  void SetNpixels(Int_t Npixels){fNpixels= Npixels;}

  void ComputeChargeDeposition(Float_t StartingSegmentSize, Float_t MaximumSegmentSize, Float_t MaximumChargePerSegment);
  void ComputeChargeTransport(DIGPlane *aDIGPlane,DIGTransport *aDIGTransport);
  void PrintInfo();

  void AddPixel(Float_t AnalogCharge, Int_t PixelNumber);
  void UpdatePixel(Float_t AnalogCharge, Int_t PixelNumber);
  void AddRandomNoise(DIGPlane *myDIGPlane);
  void AnalogToDigitalconversion(DIGADC *myDIGADC, DIGPlane *myDIGPlane);
  Int_t GetPixelNumber(DIGPlane *myDIGPlane, Float_t Xpos, Float_t Ypos);
  void GetXYPixelNumber(Int_t &Xpix, Int_t &Ypix, DIGPlane *myDIGPlane, Int_t PixelNumber);
  void GetXYPixelCenter(Float_t &Xpix, Float_t &Ypix, DIGPlane *myDIGPlane, Int_t PixelNumber);

  Double_t         GaussianLaw(Double_t mean, Double_t sigma); 
  //  Double_t         Lorentz2D(Double_t *x, Double_t *par);

 protected:
 
  Float_t fEntryX;
  Float_t fEntryY;
  Float_t fEntryZ;
  Float_t fExitX;
  Float_t fExitY;
  Float_t fExitZ;
  Float_t fEnergy_deposited;
  //  Float_t fPx;
  //  Float_t fPy;
  //  Float_t fPz;

  //segments
  Int_t fNSegment;
  vector<Float_t> fSegmentX;
  vector<Float_t> fSegmentY;
  vector<Float_t> fSegmentZ;
  vector<Float_t> fSegmentCharge;
  //pixel with deposited charge list 
  Int_t fNpixels;
  vector< Int_t > fPixelMap;
  vector< Float_t > fAnalogChargeMap;
  vector< Int_t > fDigitalChargeMap; // n'a pas de sens tant qu'on a pas somme la charge de toutes les particules
  // au niveau du read out.


  ClassDef(DIGParticle,1);
};



//==============================================================================

#endif
