///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//     DIGBeam                                                                               //
//                                                                                           //
//     Class containing incident particles / beam informations                               //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include "digbeam.h"

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
ClassImp(DIGBeam)

DIGBeam::DIGBeam()  
{
  //
  // default constructor
  //

}
//______________________________________________________________________________
//  
DIGBeam::DIGBeam(Int_t RunNumber, Int_t NumberOfEvents,Float_t ParticleDensity,Float_t ThetaIncidentDeg,
		 Float_t PhiIncidentDeg, Int_t BeamParameter)  
{
  SetRunNumber(RunNumber);
  SetNumberOfEvents(NumberOfEvents);
  SetParticleDensity(ParticleDensity);
  SetThetaIncidentDeg(ThetaIncidentDeg);
  SetPhiIncidentDeg(PhiIncidentDeg);
  SetBeamOption(BeamParameter);
}
//______________________________________________________________________________
//  

DIGBeam::~DIGBeam() { // 
  // virtual destructor
  //
  //  delete fLayers;
}
//______________________________________________________________________________
//   
void DIGBeam::SetRunNumber(Int_t RunNumber){
  fRunNumber = RunNumber;
}
//______________________________________________________________________________
//  
void DIGBeam::SetNumberOfEvents(Int_t NumberOfEvents){
  fNumberOfEvents = NumberOfEvents;
}
//______________________________________________________________________________
//  
void DIGBeam::SetParticleDensity(Float_t ParticleDensity){
  fParticleDensity = ParticleDensity;
}
//______________________________________________________________________________
//  
void DIGBeam::SetThetaIncidentDeg(Float_t ThetaIncidentDeg){
  fThetaIncidentDeg = ThetaIncidentDeg;
}
//______________________________________________________________________________
//  
void DIGBeam::SetPhiIncidentDeg(Float_t PhiIncidentDeg){
  fPhiIncidentDeg = PhiIncidentDeg;
}
//______________________________________________________________________________
//   
void DIGBeam::SetBeamOption(Int_t BeamOption){
  fBeamOption = BeamOption;
}
//______________________________________________________________________________
//  

void DIGBeam::PrintInfo() {
  std::cout<<"---------BEAM properties------------- "<<endl;
  std::cout<<"fRunNumber fNumberOfEvents fParticleDensity fThetaIncidentDeg fPhiIncidentDeg fBeamOption"<<endl;
  std::cout<<fRunNumber<<" "<< fNumberOfEvents<<" "<< fParticleDensity<<" "<< fThetaIncidentDeg<<" "
	   << fPhiIncidentDeg<<" "<<fBeamOption<<endl;
}

//==============================================================================
