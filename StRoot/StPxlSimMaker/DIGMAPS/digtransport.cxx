///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//     DIGTransport                                                                          //
//                                                                                           //
//     Contains charge transport models parameters                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <digtransport.h>


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


using namespace std;

//==============================================================================
ClassImp(DIGTransport)
//______________________________________________________________________________
//  
DIGTransport::DIGTransport()  
{
  //
  // default constructor
  //
}  
//______________________________________________________________________________
//  
DIGTransport::DIGTransport(Float_t myvar)  
{
  fMyvar = myvar;
}  
//______________________________________________________________________________
//  
DIGTransport::~DIGTransport() {  
  //
  // virtual destructor
  //
}
//______________________________________________________________________________
//  
DIGTransport::DIGTransport(DIGTransport & adigtransport)  : TObject()
{
  fMyvar = adigtransport.GetMyvar();

}
//______________________________________________________________________________
//  
void DIGTransport::Clear(const Option_t *) 
{
  //  delete pointers.  fDIGParticleArray->Clear("C");

}
//______________________________________________________________________________
//  
void DIGTransport::SetLorentz2DModel_Cp0(Float_t Lorentz2DModel_Cp0){ 
  fLorentz2DModel_Cp0 = Lorentz2DModel_Cp0;
}
//______________________________________________________________________________
//  
void DIGTransport::SetLorentz2DModel_Cp1(Float_t Lorentz2DModel_Cp1){ 
  fLorentz2DModel_Cp1 = Lorentz2DModel_Cp1;
}
//______________________________________________________________________________
//  
void DIGTransport::SetRangeLimit_InPitchUnit(Float_t RangeLimit_InPitchUnit){
  fRangeLimit_InPitchUnit = RangeLimit_InPitchUnit;
}
//______________________________________________________________________________
//  
void DIGTransport::SetChargeModel(Int_t ChargeModel){
  fChargeModel = ChargeModel;
}
//______________________________________________________________________________
//  
void DIGTransport::SetGauss2DModel_sigma1_Cp0(Float_t Gauss2DModel_sigma1_Cp0){
  fGauss2DModel_sigma1_Cp0 = Gauss2DModel_sigma1_Cp0;
}
//______________________________________________________________________________
//  
void DIGTransport::SetGauss2DModel_sigma1_Cp1(Float_t Gauss2DModel_sigma1_Cp1){
  fGauss2DModel_sigma1_Cp1 = Gauss2DModel_sigma1_Cp1;
}
//______________________________________________________________________________
//  
void DIGTransport::SetGauss2DModel_sigma2_Cp0(Float_t Gauss2DModel_sigma2_Cp0){
  fGauss2DModel_sigma2_Cp0 = Gauss2DModel_sigma2_Cp0;
}
//______________________________________________________________________________
//  
void DIGTransport::SetGauss2DModel_sigma2_Cp1(Float_t Gauss2DModel_sigma2_Cp1){
  fGauss2DModel_sigma2_Cp1 = Gauss2DModel_sigma2_Cp1;
}
//______________________________________________________________________________
//  
void DIGTransport::SetGauss2DModel_weight(Float_t Gauss2DModel_weight){
  fGauss2DModel_weight = Gauss2DModel_weight;
}
//______________________________________________________________________________
//  
void DIGTransport::SetLorGaussModel_Norm1_Cp0(Float_t LorGaussModel_Norm1_Cp0){
  fLorGaussModel_Norm1_Cp0=LorGaussModel_Norm1_Cp0  ;
}
//______________________________________________________________________________
//  
void DIGTransport::SetLorGaussModel_Norm1_Cp1(Float_t LorGaussModel_Norm1_Cp1){
  fLorGaussModel_Norm1_Cp1=LorGaussModel_Norm1_Cp1  ;
}

//______________________________________________________________________________
//  
void DIGTransport::SetLorGaussModel_Norm1_Cp2(Float_t LorGaussModel_Norm1_Cp2){
  fLorGaussModel_Norm1_Cp2=LorGaussModel_Norm1_Cp2  ;
}

//______________________________________________________________________________
//  
void DIGTransport::SetLorGaussModel_sigma_Cp0(Float_t LorGaussModel_sigma_Cp0){
  fLorGaussModel_sigma_Cp0=LorGaussModel_sigma_Cp0  ;
}

//______________________________________________________________________________
//  
void DIGTransport::SetLorGaussModel_sigma_Cp1(Float_t LorGaussModel_sigma_Cp1){
  fLorGaussModel_sigma_Cp1=LorGaussModel_sigma_Cp1  ;
}

//______________________________________________________________________________
//  
void DIGTransport::SetLorGaussModel_C_Cp0(Float_t LorGaussModel_C_Cp0){
  fLorGaussModel_C_Cp0=LorGaussModel_C_Cp0  ;
}

//______________________________________________________________________________
//  
void DIGTransport::SetLorGaussModel_C_Cp1(Float_t LorGaussModel_C_Cp1){
  fLorGaussModel_C_Cp1=LorGaussModel_C_Cp1  ;
}

//______________________________________________________________________________
//  
void DIGTransport::SetLorGaussModel_Norm_Cp0(Float_t LorGaussModel_Norm_Cp0){
  fLorGaussModel_Norm_Cp0=LorGaussModel_Norm_Cp0  ;
}
//______________________________________________________________________________
//  
  void DIGTransport::SetLorGaussModel_Norm_Cp1(Float_t LorGaussModel_Norm_Cp1){
  fLorGaussModel_Norm_Cp1 = LorGaussModel_Norm_Cp1;
}
//______________________________________________________________________________
//  
void DIGTransport::PrintInfo() {
  std::cout<<"---------DIGTransport properties------------- "<<endl;
  std::cout<<"fChargeModel"<<endl;
  std::cout<<fChargeModel<<endl;
  if(fChargeModel==1){
  std::cout<<"fLorentz2DModel_Cp0"<<endl;
  std::cout<<fLorentz2DModel_Cp0<<endl;
  std::cout<<"fLorentz2DModel_Cp1"<<endl;
  std::cout<<fLorentz2DModel_Cp1<<endl;
  std::cout<<"fRangeLimit_InPitchUnit"<<endl;
  std::cout<<fRangeLimit_InPitchUnit<<endl;
  }else if(fChargeModel==2){

  std::cout<<"fGauss2DModel_sigma1_Cp0"<<endl;
  std::cout<<fGauss2DModel_sigma1_Cp0  <<endl;
  std::cout<<"fGauss2DModel_sigma1_Cp1"<<endl;
  std::cout<< fGauss2DModel_sigma1_Cp1 <<endl;
  std::cout<<"fGauss2DModel_sigma2_Cp0"<<endl;
  std::cout<<fGauss2DModel_sigma2_Cp0  <<endl;
  std::cout<<"fGauss2DModel_sigma2_Cp1"<<endl;
  std::cout<< fGauss2DModel_sigma2_Cp1 <<endl;
  std::cout<<"fGauss2DModel_weight"<<endl;
  std::cout<< fGauss2DModel_weight <<endl;
  }



  //std::cout<<""<<endl;
  //std::cout<<<<endl;
}

//______________________________________________________________________________
//  
void DIGTransport::SetMyvar(Float_t Myvar){
  fMyvar=Myvar;
}  
//______________________________________________________________________________
//  
