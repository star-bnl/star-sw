#ifndef MAPS_DIGPLANE_H
#define MAPS_DIGPLANE_H

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
class DIGPlane : public TObject {
 public:
  DIGPlane();
  //  DIGPlane(char *name,char *title);
  virtual ~DIGPlane();

  //  void AddLayer(char *name, Float_t radius, Float_t radL, Float_t phiRes=99999, Float_t zRes=99999, Float_t integrationTime=-1.);


  void SetDimensions(Float_t Xdimension,Float_t Ydimension,Float_t Zdimension);
  void SetPitch(Float_t PitchX,Float_t PitchY);
  void SetNpixels(Int_t NpixelsX,Int_t NpixelsY);
  void SetNoiseElectrons(Float_t NoiseElectrons);
  void SetTemperature(Float_t Temperature);
  void SetIonizationEnergy(Float_t IonizationEnergy);
  void SetSegmentSize(Float_t SegmentSize);
  void SetMaximumSegmentSize(Float_t MaximumSegmentSize);
  void SetMaximumChargePerSegment(Float_t MaximumChargePerSegment);
  void SetDiffusionMaximumRange(Float_t DiffusionMaximumRangeInX,Float_t DiffusionMaximumRangeInY);
  void SetReflexionCoefficient(Float_t ReflexionCoefficient);
  void SetBasicModel_SigmaTenMicrons(Float_t BasicModel_SigmaTenMicrons);
  // void Set(Float_t ,Float_t );


  void PrintInfo();
  Float_t GetPitchX(){return fPitchX;}
  Float_t GetPitchY(){return fPitchY;}
  Float_t GetXdimension(){return fXdimension;}
  Float_t GetYdimension(){return fYdimension;}
  Float_t GetZdimension(){return fZdimension;}
  Int_t GetNpixelsX(){return fNpixelsX;}
  Int_t GetNpixelsY(){return fNpixelsY;}
  Float_t GetNoiseElectrons(){return fNoiseElectrons;}
  Float_t GetTemperature(){return fTemperature;}
  Float_t GetIonizationEnergy(){return fIonizationEnergy;}
  Float_t GetSegmentSize(){return fSegmentSize;}
  Float_t GetMaximumSegmentSize(){return fMaximumSegmentSize;}
  Float_t GetMaximumChargePerSegment(){return fMaximumChargePerSegment;}
  Float_t GetDiffusionMaximumRangeInX(){return fDiffusionMaximumRangeInX;}
  Float_t GetDiffusionMaximumRangeInY(){return fDiffusionMaximumRangeInY;}
  Float_t GetReflexionCoefficient(){return fReflexionCoefficient;}
  Float_t GetBasicModel_SigmaTenMicrons(){return fBasicModel_SigmaTenMicrons;}

  //  Float_t Get(){return ;}


 protected:
  Float_t fPitchX;
  Float_t fPitchY;
  Float_t fXdimension;
  Float_t fYdimension;
  Float_t fZdimension; //epitaxial thickness
  Int_t fNpixelsX;
  Int_t fNpixelsY;

  Float_t   fNoiseElectrons;
  Float_t   fTemperature;
  Float_t   fIonizationEnergy;
  Float_t   fSegmentSize;
  Float_t   fMaximumSegmentSize;
  Float_t   fMaximumChargePerSegment;
  Float_t   fDiffusionMaximumRangeInX;
  Float_t   fDiffusionMaximumRangeInY;
  Float_t   fReflexionCoefficient;
  Float_t   fBasicModel_SigmaTenMicrons;

  /* Int_t numberOfLayers;
     TList fLayers; 
     Float_t fBField;
     Collider fCollider;
     Float_t fGeomAcceptance;
     Float_t fIncidentPolarAngle;
     
     enum {MaxNumberOfDetectors = 300};
     
     Double_t xpoint[400], ypoint[400], yprime[400], yprimeZ[400], equivalent[400] ;
     Double_t DetPointRes[MaxNumberOfDetectors][400], DetPointZRes[MaxNumberOfDetectors][400] ;
     Double_t ratio[400], PXLReference[400], efficiency[2][400];
  */

  ClassDef(DIGPlane,1);
};





//==============================================================================

#endif
