///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//     DIGADC                                                                                //
//                                                                                           //
//    Class containing the ADC/discri features                                               //
//          (Nbits,  thresholds, etc.)                                                       //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include "digadc.h"
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
ClassImp(DIGADC)

DIGADC::DIGADC()  
{
  //
  // default constructor
  //

}
//______________________________________________________________________________
//  

DIGADC::~DIGADC() { // 
  // virtual destructor
  //
  //  delete fLayers;
}
//______________________________________________________________________________
//  
void DIGADC::SetNbits(Int_t Nbits){
  fNbits = Nbits;
}
//______________________________________________________________________________
//  
void DIGADC::SetNThresholds(Int_t NThresholds){
  fNThresholds = NThresholds;
}
//______________________________________________________________________________
//  
void DIGADC::SetADC_linear(Bool_t ADC_linear){
  fADC_linear = ADC_linear;
}
//______________________________________________________________________________
//  
void DIGADC::SetLSB(Float_t LSB){
  fLSB = LSB;
}
//______________________________________________________________________________
//  
void DIGADC::SetElectron_Conversion(Float_t Electron_Conversion){
  fElectron_Conversion = Electron_Conversion;
}
//______________________________________________________________________________
//  
void DIGADC::SetADC_thresholds(Float_t ADC_thresholds[], Int_t NThresholds){
  for (Int_t i = 0; i <NThresholds ; i++){
    fADC_thresholds[i] = ADC_thresholds[i];
  }
}
//______________________________________________________________________________
//  
void DIGADC::PrintInfo(){
  std::cout<<"-------- ADC INFOS "<<endl;
  std::cout<<"fNbits fNThresholds fADC_linear fLSB fElectron_Conversion"<<endl;
  std::cout<<fNbits<<" "<< fNThresholds<< " "<< fADC_linear<<" "<< fLSB<<" "<< fElectron_Conversion<<endl;
  Int_t Nthtoprint = fNThresholds;
  if(Nthtoprint < 33){
    for (Int_t i = 0; i <Nthtoprint; i++){
      std::cout<<i<<" "<<fADC_thresholds[i] <<endl;
    }
  }else{
    Nthtoprint = 32;
    for (Int_t i = 0; i <Nthtoprint; i++){
      std::cout<<i<<" "<<fADC_thresholds[i] <<endl;
    }
    std::cout<<" etc. "<<endl;  
  }
}
//______________________________________________________________________________
//  

//==============================================================================
