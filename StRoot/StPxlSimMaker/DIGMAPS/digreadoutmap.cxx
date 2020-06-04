///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//     DIGReadoutmap                                                                         //
//                                                                                           //
//       final output of the event                                                           //
//          -list of pixels with a collected charge >0                                       //
//          -Analog charge list                                                              //
//          -Digital charge list                                                             //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <digreadoutmap.h>


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
ClassImp(DIGReadoutmap)
//______________________________________________________________________________
//  
DIGReadoutmap::DIGReadoutmap()  
{
  fNpixels=0;
  //
  // default constructor
  //
}  
//______________________________________________________________________________
//  
DIGReadoutmap::DIGReadoutmap(Int_t Npixels)  
{
  fNpixels = Npixels;
  fPixelMap.resize(fNpixels);
  fAnalogChargeMap.resize(fNpixels);
  fDigitalChargeMap.resize(fNpixels);
}  
//______________________________________________________________________________
//  
DIGReadoutmap::~DIGReadoutmap() {  
  //
  // virtual destructor

}
//______________________________________________________________________________
//  
DIGReadoutmap::DIGReadoutmap(DIGReadoutmap & adigreadoutmap)  : TObject()
{
  //copy constructor:
  fNpixels = adigreadoutmap.GetNpixels();
  fPixelMap.resize(fNpixels);
  fAnalogChargeMap.resize(fNpixels);
  fDigitalChargeMap.resize(fNpixels);
  for (Int_t i=0 ; i<fNpixels ; i++){
    fPixelMap[i] = adigreadoutmap.GetPixelMap()[i];
    fAnalogChargeMap[i] = adigreadoutmap.GetAnalogCharge()[i];
    fDigitalChargeMap[i] = adigreadoutmap.GetDigitalCharge()[i];
  }
}
//______________________________________________________________________________
//  
void DIGReadoutmap::Clear(const Option_t *) 
{
  //  delete pointers.  fDIGParticleArray->Clear("C");

}
//______________________________________________________________________________
//  
void DIGReadoutmap::PrintInfo() {
  std::cout<<"---------DIGReadoutmap properties------------- "<<endl;
  std::cout<<fNpixels<<" fNpixels map analog digital "<<endl;
  if(fNpixels>0){
    std::cout<<" size vectors "<<  fPixelMap.size()<<" "<<fAnalogChargeMap.size()<<" "<<fDigitalChargeMap.size()<<endl;
    for (Int_t i=0 ; i<fNpixels ; i++){
      std::cout<<i<<" "<<fPixelMap[i]<<" "<<fAnalogChargeMap[i]<<" "<<fDigitalChargeMap[i]<<endl;
    } 
  }
  std::cout<<"---------END OF DIGReadoutmap properties------------- "<<endl;
}
//______________________________________________________________________________
//  
void DIGReadoutmap::PrintOuput(Int_t Nx ,Int_t Ny) {
  std::cout<<"---------DIGReadoutmap PrintOuput------------- "<<endl;
  std::cout<<fNpixels<<" fNpixels  "<<endl;
  if(fNpixels>0){    
    std::cout<<" size vectors "<<  fPixelMap.size()<<" "<<fAnalogChargeMap.size()<<" "<<fDigitalChargeMap.size()<<endl;
    for (Int_t iy=0 ; iy<Ny ; iy++){
      for (Int_t ix=0 ; ix<Nx ; ix++){
	//search pixel:
	Int_t Npixel = ix+Nx*iy;
	Bool_t found = false;
	Int_t j=0;
	while((!found)&&(j<fNpixels)){
	  if(Npixel==fPixelMap[j]){
	    found=true;
	    cout<<" "<<fDigitalChargeMap[j];
	  }else{
	    j++;
	  }
	}
	if(!found){
	  cout<<" -";
	}
      }
      cout<<endl;
    }
  }
  
  std::cout<<"---------END OF DIGReadoutmap PrintOuput------------- "<<endl;
}
//______________________________________________________________________________
//  
void DIGReadoutmap::AddPixel(Float_t AnalogCharge, Int_t PixelNumber) {
  fNpixels++;
  fPixelMap.push_back(PixelNumber);
  fAnalogChargeMap.push_back(AnalogCharge);
  fDigitalChargeMap.push_back(0);
}
//______________________________________________________________________________
//  
void DIGReadoutmap::UpdatePixel(Float_t AnalogCharge, Int_t PixelNumber) {
  Bool_t found = false;
  Int_t i=0;
  while((!found)&&(i<fNpixels)){
    if(PixelNumber==fPixelMap[i]){
      found=true;
      fAnalogChargeMap[i]+=AnalogCharge;
    }
    i++;
  }
  if(!found){
    AddPixel(AnalogCharge, PixelNumber);
  } 
}

//______________________________________________________________________________
//  
void DIGReadoutmap::AnalogToDigitalconversion(DIGADC *myDIGADC,  DIGPlane *myDIGPlane ){
  Double_t Noisefix = myDIGPlane->GetNoiseElectrons();
  if(Noisefix<=0.0){
    std::cout <<"<---- DIGReadoutmap::AnalogToDigitalconversion --->"<<endl;
    std::cout<<"WARNING negative or null Noise is not physical, please correct the input file"<<endl; 
    Noisefix = 1.0;
  }
  for (Int_t i = 0; i < fNpixels ; i++){
    if (fAnalogChargeMap[i]<=0.0){
      fDigitalChargeMap[i]=0;
    }else{
      Bool_t thresholdfound = false;
      Int_t ithres = 0;      
      fDigitalChargeMap[i]=0;
      while((thresholdfound==false)&&(ithres< (myDIGADC->GetNThresholds()) )){
	if( (fAnalogChargeMap[i]/Noisefix) < myDIGADC->GetADC_thresholds()[ithres] ){
	  thresholdfound = true;
	}else{
	  fDigitalChargeMap[i]++;
	  ithres++;
	}
      }
    }
  }

}
//______________________________________________________________________________
//   
