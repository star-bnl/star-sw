///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//                                                                                           //
//      DIGPlane                                                                             //
//                                                                                           //
//       Plane/Chip class                                                                    //
//          contains geometrical information on the chip                                     //
//              (number of pixels, pitch, epitaxial layer thickness etc.)                    //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <digplane.h>

#include <TROOT.h> // for gROOT object
#include <TMath.h>
#include <TMatrixD.h>
#include <TCanvas.h>
#include <TGraph.h>
#include <TAxis.h>
#include <TRandom3.h>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>
#include <TClonesArray.h>
using namespace std;

//==============================================================================
ClassImp(DIGPlane)

DIGPlane::DIGPlane()  
{
  //
  // default constructor
  //
  fPitchX = 0.0;
  fPitchY = 0.0;
  fXdimension = 0.0;
  fYdimension = 0.0;
  fZdimension = 0.0;
  fNpixelsX = 0;
  fNpixelsY = 0;
}
//______________________________________________________________________________
//  

DIGPlane::~DIGPlane() { // 
  // virtual destructor
  //
  //  delete fLayers;
}

//______________________________________________________________________________
//  

void DIGPlane::SetDimensions(Float_t Xdimension,Float_t Ydimension,Float_t Zdimension) {
  //
  // Set dimensions of the Plane
  //
  fXdimension = Xdimension;
  fYdimension = Ydimension;
  fZdimension = Zdimension;
}

//______________________________________________________________________________
//  
void DIGPlane::SetPitch(Float_t PitchX,Float_t PitchY){
  fPitchX = PitchX;
  fPitchY = PitchY;
}
//______________________________________________________________________________
//  
void DIGPlane::SetNpixels(Int_t NpixelsX,Int_t NpixelsY){
  fNpixelsX = NpixelsX;
  fNpixelsY = NpixelsY;
}
//______________________________________________________________________________
//  
void DIGPlane::SetNoiseElectrons(Float_t NoiseElectrons){
  fNoiseElectrons = NoiseElectrons;
}
//______________________________________________________________________________
//  
void DIGPlane::SetTemperature(Float_t Temperature){
  fTemperature = Temperature;
}
//______________________________________________________________________________
//  
void DIGPlane::SetIonizationEnergy(Float_t IonizationEnergy){
  fIonizationEnergy = IonizationEnergy;
}
//______________________________________________________________________________
//  
void DIGPlane::SetSegmentSize(Float_t SegmentSize){
  fSegmentSize = SegmentSize;
}
//______________________________________________________________________________
//  
void DIGPlane::SetMaximumSegmentSize(Float_t MaximumSegmentSize){
  fMaximumSegmentSize = MaximumSegmentSize;
}
//______________________________________________________________________________
//  
void DIGPlane::SetMaximumChargePerSegment(Float_t MaximumChargePerSegment){
  fMaximumChargePerSegment = MaximumChargePerSegment;
}
//______________________________________________________________________________
//  
void DIGPlane::SetDiffusionMaximumRange(Float_t DiffusionMaximumRangeInX,Float_t DiffusionMaximumRangeInY){
  fDiffusionMaximumRangeInX = DiffusionMaximumRangeInX;
  fDiffusionMaximumRangeInY = DiffusionMaximumRangeInY;
}
//______________________________________________________________________________
//  
void DIGPlane::SetReflexionCoefficient(Float_t ReflexionCoefficient){
  fReflexionCoefficient = ReflexionCoefficient;
}
//______________________________________________________________________________
//  
void DIGPlane::SetBasicModel_SigmaTenMicrons(Float_t BasicModel_SigmaTenMicrons){
  fBasicModel_SigmaTenMicrons = BasicModel_SigmaTenMicrons;
}
//______________________________________________________________________________
//  

void DIGPlane::PrintInfo() {
  std::cout<<"---------Plane properties------------- "<<endl;
  std::cout<<"fPitchX fPitchY fXdimension fYdimension fZdimension  "<<endl;
  std::cout<<fPitchX<<" "<< fPitchY<<" "<< fXdimension<<" "<< fYdimension<<" "<< fZdimension  <<endl;
  std::cout<<"fNpixelsX fNpixelsY fNoiseElectrons fTemperature  "<<endl;
  std::cout<<fNpixelsX<<" "<< fNpixelsY<<" "<< fNoiseElectrons<<" "<< fTemperature  <<endl;
  std::cout<<"fIonizationEnergy fSegmentSize fMaximumSegmentSize fMaximumChargePerSegment "<<endl;
  std::cout<<fIonizationEnergy <<" "<<fSegmentSize<<" "<< fMaximumSegmentSize<<" "<< fMaximumChargePerSegment <<endl;
  std::cout<<"fDiffusionMaximumRangeInX fDiffusionMaximumRangeInY fReflexionCoefficient "<<endl; 
  std::cout<<fDiffusionMaximumRangeInX<<" "<< fDiffusionMaximumRangeInY <<" "<<fReflexionCoefficient <<endl; 
  std::cout<<"fBasicModel_SigmaTenMicrons"<<endl;
  std::cout<<fBasicModel_SigmaTenMicrons<<endl;

}


//==============================================================================
