///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//      DIGCluster                                                                           //
//                                                                                           //
//      Class containing   cluster information                                               //
//        (pixel list, digital charge)                                                       //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <digcluster.h>


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

//include other classes.h:
#include "digplane.h"
#include "digadc.h"


using namespace std;

//==============================================================================
ClassImp(DIGCluster)
//______________________________________________________________________________
//  
DIGCluster::DIGCluster()  
{
  //
  // default constructor
  //
  fNpixels=0;
  fPixelMap.clear();
  fDigitalChargeMap.clear();
  Xposition_CoG =-2.0;
  Yposition_CoG =-2.0;
  fSeedPixelIndex = -1;
}  
//______________________________________________________________________________
//  
DIGCluster::~DIGCluster() {  
  //
  // virtual destructor
  //
}
//______________________________________________________________________________
//  
DIGCluster::DIGCluster(DIGCluster & adigCluster)  : TObject()
{

  //recopy constructor
  fNpixels = adigCluster.GetNpixels();

  fPixelMap.resize((adigCluster.GetPixelMap()).size());
  fDigitalChargeMap.resize((adigCluster.GetPixelMap()).size());
  for (Int_t i=0 ; i<fNpixels ; i++){
    fPixelMap[i] = adigCluster.GetPixelMap()[i];
    fDigitalChargeMap[i] = adigCluster.GetDigitalCharge()[i];
  }
  Xposition_CoG = adigCluster.GetXposition_CoG();
  Yposition_CoG = adigCluster.GetYposition_CoG();
  fSeedPixelIndex = adigCluster.GetSeedPixelIndex();

}
//______________________________________________________________________________
//  
void DIGCluster::Clear(const Option_t *) 
{
  //  delete pointers.  fDIGParticleArray->Clear("C");

}
//______________________________________________________________________________
//   
Int_t DIGCluster::GetTotalCharge(){
  Int_t TotalCharge = 0;
  for (Int_t i=0 ; i<fNpixels ; i++){
    TotalCharge+=fDigitalChargeMap[i];
  }
  return TotalCharge;
}
//______________________________________________________________________________
//  
Int_t DIGCluster::Get1stCrownCharge(DIGPlane *myDIGPlane){
  Int_t f1stCrownCharge = 0;
  std::vector<Int_t> pixmapvector;
  pixmapvector = Get1stCrownPixelsIndex(myDIGPlane);
  Int_t imax = pixmapvector.size();
  for ( Int_t i=0 ; i<imax ; i++ ) {
    for ( Int_t j=0 ; j<fNpixels; j++ ) {
      if(pixmapvector[i]==fPixelMap[j]){
	f1stCrownCharge+=fDigitalChargeMap[j];
      }
      //  cout<<" DIGCluster::Get1stCrownCharge i imax pixmapvector "<<i<<" "<<imax<<" "<<pixmapvector[i]<<endl;
      //  cout<<" j fNpixels fPixelMap "<< j<<" "<<fNpixels<<" "<<" "<<fPixelMap[j]<<endl;
      //   cout<<"f1stCrownCharge = "<<f1stCrownCharge<<endl;
    }
  }
  //cout<<" DIGCluster::Get1stCrownCharge "<<f1stCrownCharge<<endl;
  return f1stCrownCharge;
}
//______________________________________________________________________________
//  
Int_t DIGCluster::Get2ndCrownCharge(DIGPlane *myDIGPlane){
  Int_t f2ndCrownCharge = 0;
  std::vector<Int_t> pixmapvector;
  pixmapvector = Get2ndCrownPixelsIndex(myDIGPlane);
  Int_t imax = pixmapvector.size();
  for ( Int_t i=0 ; i<imax;  i++ ) {
    for ( Int_t j=0 ; j<fNpixels;  j++ ) {
      if(pixmapvector[i]==fPixelMap[j]){
	f2ndCrownCharge+=fDigitalChargeMap[j];
      }
    }
  }
  return f2ndCrownCharge;

}
//______________________________________________________________________________
//  
Int_t DIGCluster::Get4NeigboursCharge(DIGPlane *myDIGPlane){
  Int_t f4NeigboursCharge = 0;
  std::vector<Int_t> pixmapvector;
  pixmapvector = Get4NeigboursPixelsIndex(myDIGPlane);
  Int_t imax = pixmapvector.size();
  for ( Int_t i=0 ; i<imax ; i++ ) {
    for ( Int_t j=0 ; j<fNpixels;  j++ ) {
      if(pixmapvector[i]==fPixelMap[j]){
	f4NeigboursCharge+=fDigitalChargeMap[j];
      }
    }
  }
  return f4NeigboursCharge;
}
//______________________________________________________________________________
//  
Int_t DIGCluster::GetMultiplicity(Int_t Threshold){
  Int_t TotalMultiplicity = 0;
  for (Int_t i=0 ; i<fNpixels ; i++){
    if(fDigitalChargeMap[i]>=Threshold){
      TotalMultiplicity++;
    }
  }
  return TotalMultiplicity;

  
}
//______________________________________________________________________________
//  
void DIGCluster::Compute_CoG(DIGPlane *myDIGPlane){
  
  Double_t XCoG = 0.0;
  Double_t YCoG = 0.0;
  Double_t TotalCharge = 0.0;
  for (Int_t i = 0; i < fNpixels ; i++){
    Int_t PixelNumber = fPixelMap[i];
    Double_t Xpix = (myDIGPlane->GetPitchY()) * (0.5+PixelNumber%(myDIGPlane->GetNpixelsX()));
    Double_t Ypix = (myDIGPlane->GetPitchX()) * (0.5+PixelNumber/(myDIGPlane->GetNpixelsX()));
    Int_t Charge = fDigitalChargeMap[i];
    XCoG += Charge * Xpix;
    YCoG += Charge * Ypix;
    TotalCharge += Charge;
  }
  if(TotalCharge>0){
  XCoG = XCoG / TotalCharge;
  YCoG = YCoG / TotalCharge;
  }else{
    XCoG = -1.0;
    YCoG = -1.0;
  }
  SetXposition_CoG(XCoG);
  SetYposition_CoG(YCoG);
}
//______________________________________________________________________________
//   

void DIGCluster::AddPixel(Int_t DigitalCharge, Int_t PixelNumber){
  fNpixels++;
  fPixelMap.push_back(PixelNumber);
  fDigitalChargeMap.push_back(DigitalCharge);
}
//______________________________________________________________________________
//  
void DIGCluster::PrintInfo() {
  std::cout<<"---------DIGCluster properties------------- "<<endl;
  std::cout<<" Total charge, mul1, mul2, mul3, mul4 "<<endl;
  std::cout<<GetTotalCharge()
	   <<" "<<GetMultiplicity(1)
	   <<" "<<GetMultiplicity(2)
	   <<" "<<GetMultiplicity(3)
	   <<" "<<GetMultiplicity(4)<<endl;
  for (Int_t i=0 ; i<fNpixels ; i++){
    std::cout<<i<<" "<<fPixelMap[i]<<" "<<" "<<fDigitalChargeMap[i]<<endl;
  }
  std::cout<<" CoG "<<GetXposition_CoG() <<" "<< GetYposition_CoG()<<endl;
  std::cout<<" SEED index, totalC  seedcharge "<< GetSeedPixelIndex()<<" "<<GetTotalCharge()<<" "<<GetDigitalCharge()[GetSeedPixelIndex()]<<endl;

}
//______________________________________________________________________________
//  
void DIGCluster::Compute_SeedPixel(DIGPlane *myDIGPlane){
 // compute the seed pixel.
  // Find the highest charge and if there are several pixels with the maximum charge, chose the pixel which is the closest of the other pixels.

  Int_t CurrentDigSeedCharge = 0;
  Int_t CurrentDigSeedIndex = -1;
  Float_t CurrentDist = 0;
  for (Int_t i = 0; i < fNpixels ; i++){
    if(fDigitalChargeMap[i]>CurrentDigSeedCharge){
      CurrentDigSeedCharge = fDigitalChargeMap[i];
      CurrentDigSeedIndex = i;
      for (Int_t j = 0; j < fNpixels ; j++){
	if(fDigitalChargeMap[j]==CurrentDigSeedCharge){
	  Int_t SeedCandidatePixelNumber = fPixelMap[i];
	  Double_t SeedCandidateXpix = (myDIGPlane->GetPitchY()) * (0.5+SeedCandidatePixelNumber%(myDIGPlane->GetNpixelsX()));
	  Double_t SeedCandidateYpix = (myDIGPlane->GetPitchX()) * (0.5+SeedCandidatePixelNumber/(myDIGPlane->GetNpixelsX()));
	  Int_t PixelNumber = fPixelMap[j];
	  Double_t Xpix = (myDIGPlane->GetPitchY()) * (0.5+PixelNumber%(myDIGPlane->GetNpixelsX()));
	  Double_t Ypix = (myDIGPlane->GetPitchX()) * (0.5+PixelNumber/(myDIGPlane->GetNpixelsX()));	  
	  CurrentDist += TMath::Sqrt((Xpix-SeedCandidateXpix)*(Xpix-SeedCandidateXpix)+(Ypix-SeedCandidateYpix)*(Ypix-SeedCandidateYpix));
	}
      }
    }else if(fDigitalChargeMap[i]==CurrentDigSeedCharge){
      Float_t newdist = 0;
      for (Int_t j = 0; j < fNpixels ; j++){
	if(fDigitalChargeMap[j]==CurrentDigSeedCharge){
	  Int_t SeedCandidatePixelNumber = fPixelMap[i];
	  Double_t SeedCandidateXpix = (myDIGPlane->GetPitchY()) * (0.5+SeedCandidatePixelNumber%(myDIGPlane->GetNpixelsX()));
	  Double_t SeedCandidateYpix = (myDIGPlane->GetPitchX()) * (0.5+SeedCandidatePixelNumber/(myDIGPlane->GetNpixelsX()));
	  Int_t PixelNumber = fPixelMap[j];
	  Double_t Xpix = (myDIGPlane->GetPitchY()) * (0.5+PixelNumber%(myDIGPlane->GetNpixelsX()));
	  Double_t Ypix = (myDIGPlane->GetPitchX()) * (0.5+PixelNumber/(myDIGPlane->GetNpixelsX()));	  
	  newdist += TMath::Sqrt((Xpix-SeedCandidateXpix)*(Xpix-SeedCandidateXpix)+(Ypix-SeedCandidateYpix)*(Ypix-SeedCandidateYpix));
	}
      }
      if(newdist<CurrentDist){
	CurrentDigSeedCharge = fDigitalChargeMap[i];
	CurrentDigSeedIndex = i;
	CurrentDist = newdist;
      }
    }
  }
  SetSeedPixelIndex(CurrentDigSeedIndex);

}
//______________________________________________________________________________
//   
void DIGCluster::GetXYPixelNumber(Int_t &Xpix, Int_t &Ypix, DIGPlane *myDIGPlane, Int_t PixelNumber){

  Xpix = PixelNumber%(myDIGPlane->GetNpixelsX());
  Ypix = PixelNumber/(myDIGPlane->GetNpixelsX());

}
//______________________________________________________________________________
//  
Int_t DIGCluster::GetSeedPixelIndex(){
//return the seed pixel index in the pixel map.
  return fSeedPixelIndex;
}
//______________________________________________________________________________
//  
void DIGCluster::SetSeedPixelIndex(Int_t Index)
{
  fSeedPixelIndex = Index;
}
//______________________________________________________________________________
//  
std::vector<Int_t> DIGCluster::Get4NeigboursPixelsIndex(DIGPlane *myDIGPlane){
  vector< Int_t > PixelMap;
  PixelMap.clear();
  Int_t Xpix;
  Int_t Ypix;
  GetXYPixelNumber(Xpix,Ypix,myDIGPlane,fPixelMap[fSeedPixelIndex]); // <--------------------------------
  if(IsPixelInThePlane(Xpix+1,Ypix,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+1);
  }
  if(IsPixelInThePlane(Xpix-1,Ypix,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-1);
  }
  if(IsPixelInThePlane(Xpix,Ypix+1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix,Ypix-1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-myDIGPlane->GetNpixelsX());
  }
  return PixelMap;
}
//______________________________________________________________________________
//  
std::vector<Int_t> DIGCluster::Get1stCrownPixelsIndex(DIGPlane *myDIGPlane){
  vector< Int_t > PixelMap;
  PixelMap.clear();
  Int_t Xpix;
  Int_t Ypix;
  GetXYPixelNumber(Xpix,Ypix,myDIGPlane,fPixelMap[fSeedPixelIndex]);
  if(IsPixelInThePlane(Xpix-1,Ypix-1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-1-myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix,Ypix-1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix+1,Ypix-1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+1-myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix-1,Ypix,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-1);
  }
  if(IsPixelInThePlane(Xpix+1,Ypix,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+1);
  }
  if(IsPixelInThePlane(Xpix-1,Ypix+1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-1+myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix,Ypix+1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix+1,Ypix+1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+1+myDIGPlane->GetNpixelsX());
  }
  return PixelMap;
}
//______________________________________________________________________________
//  
std::vector<Int_t> DIGCluster::Get2ndCrownPixelsIndex(DIGPlane *myDIGPlane){
  vector< Int_t > PixelMap;
  PixelMap.clear();
  Int_t Xpix;
  Int_t Ypix;
  GetXYPixelNumber(Xpix,Ypix,myDIGPlane,fPixelMap[fSeedPixelIndex]);
  if(IsPixelInThePlane(Xpix-2,Ypix-2,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-2-2*myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix-1,Ypix-2,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-1-2*myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix,Ypix-2,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-2*myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix+1,Ypix-2,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+1-2*myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix+2,Ypix-2,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+2-2*myDIGPlane->GetNpixelsX());
  }


  if(IsPixelInThePlane(Xpix-2,Ypix-1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-2-myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix+2,Ypix-1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+2-myDIGPlane->GetNpixelsX());
  }

  if(IsPixelInThePlane(Xpix-2,Ypix,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-2);
  }
  if(IsPixelInThePlane(Xpix+2,Ypix,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+2);
  }


  if(IsPixelInThePlane(Xpix-2,Ypix+1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-2+myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix+2,Ypix+1,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+2+myDIGPlane->GetNpixelsX());
  }

  if(IsPixelInThePlane(Xpix-2,Ypix+2,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-2+2*myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix-1,Ypix+2,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]-1+2*myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix,Ypix+2,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+2*myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix+1,Ypix+2,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+1+2*myDIGPlane->GetNpixelsX());
  }
  if(IsPixelInThePlane(Xpix+2,Ypix+2,myDIGPlane)){
    PixelMap.push_back(fPixelMap[fSeedPixelIndex]+2+2*myDIGPlane->GetNpixelsX());
  }


  return PixelMap;
}
//______________________________________________________________________________
//  
Bool_t DIGCluster::IsPixelInThePlane(Int_t Xpix, Int_t Ypix, DIGPlane *myDIGPlane){

  Bool_t IsIn = true;
  Int_t NpixelsX = myDIGPlane->GetNpixelsX();
  Int_t NpixelsY = myDIGPlane->GetNpixelsY();
  if(Xpix<0){IsIn = false;}
  if(Ypix<0){IsIn = false;}
  if(Xpix>=NpixelsX){IsIn = false;}
  if(Ypix>=NpixelsY){IsIn = false;}
  return IsIn;
}


 
