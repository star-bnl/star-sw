#ifndef MAPS_BEAM_H
#define MAPS_BEAM_H

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
class DIGBeam : public TObject {
 public:
  DIGBeam();
  DIGBeam(Int_t RunNumber,Int_t NumberOfEvents,Float_t ParticleDensity,Float_t ThetaIncidentDeg,Float_t PhiIncidentDeg,
	  Int_t BeamParameter);
  //  DIGPlane(char *name,char *title);
  virtual ~DIGBeam();
  void SetRunNumber(Int_t RunNumber);
  void SetNumberOfEvents(Int_t NumberOfEvents);
  void SetParticleDensity(Float_t ParticleDensity);
  void SetThetaIncidentDeg(Float_t ThetaIncidentDeg);
  void SetPhiIncidentDeg(Float_t PhiIncidentDeg);
  void SetBeamOption(Int_t BeamOption);
  void PrintInfo();

  Int_t GetRunNumber(){return fRunNumber;}
  Int_t GetNumberOfEvents(){return fNumberOfEvents;}
  Float_t GetParticleDensity(){return fParticleDensity;}
  Float_t GetThetaIncidentDeg(){return fThetaIncidentDeg;}
  Float_t GetPhiIncidentDeg(){return fPhiIncidentDeg;}
  Int_t GetBeamOption(){return fBeamOption;}

 protected:
 
  Int_t fBeamOption;
  Int_t fRunNumber;
  Int_t fNumberOfEvents;
  Float_t fParticleDensity;
  Float_t fThetaIncidentDeg;
  Float_t fPhiIncidentDeg;


  ClassDef(DIGBeam,1);
};




//==============================================================================

#endif
