///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//        DIGProto                                                                           //
//        Dummy class                                                                        //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <digproto.h>


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
ClassImp(DIGProto)
//______________________________________________________________________________
//  
DIGProto::DIGProto()  
{
  //
  // default constructor
  //
}  
//______________________________________________________________________________
//  
DIGProto::DIGProto(Float_t myvar)  
{
  fMyvar = myvar;
}  
//______________________________________________________________________________
//  
DIGProto::~DIGProto() {  
  //
  // virtual destructor
  //
}
//______________________________________________________________________________
//  
DIGProto::DIGProto(DIGProto & adigproto)  : TObject()
{
  fMyvar = adigproto.GetMyvar();

}
//______________________________________________________________________________
//  
void DIGProto::Clear(const Option_t *) 
{
  //  delete pointers.  fDIGParticleArray->Clear("C");

}
//______________________________________________________________________________
//  
void DIGProto::PrintInfo() {
  std::cout<<"---------DIGProto properties------------- "<<endl;
  //std::cout<<"fEntryX fEntryY fExitX fExitY fEnergy_deposited"<<endl;
  // std::cout<<fEntryX<<" "<< fEntryY<<" "<<fExitX <<" "<<fExitY <<" "<<fEnergy_deposited <<endl;
}
//______________________________________________________________________________
//  
void DIGProto::SetMyvar(Float_t Myvar){
  fMyvar=Myvar;
}  
//______________________________________________________________________________
//  
