///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//        DIGResult                                                                           //
//        Dummy class                                                                        //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <digresult.h>


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
ClassImp(DIGResult)
//______________________________________________________________________________
//  
DIGResult::DIGResult()  
{
  //
  // default constructor
  //
}  
//______________________________________________________________________________
//  
DIGResult::DIGResult(Float_t myvar)  
{
  fMyvar = myvar;
}  
//______________________________________________________________________________
//  
DIGResult::~DIGResult() {  
  //
  // virtual destructor
  //
}
//______________________________________________________________________________
//  
DIGResult::DIGResult(DIGResult & adigresult)  : TObject()
{
  fMyvar = adigresult.GetMyvar();

}
//______________________________________________________________________________
//  
void DIGResult::Clear(const Option_t *) 
{
  //  delete pointers.  fDIGParticleArray->Clear("C");

}
//______________________________________________________________________________
//  
void DIGResult::PrintInfo() {
  std::cout<<"---------DIGResult properties------------- "<<endl;
  //std::cout<<"fEntryX fEntryY fExitX fExitY fEnergy_deposited"<<endl;
  // std::cout<<fEntryX<<" "<< fEntryY<<" "<<fExitX <<" "<<fExitY <<" "<<fEnergy_deposited <<endl;
}
//______________________________________________________________________________
//  
void DIGResult::SetMyvar(Float_t Myvar){
  fMyvar=Myvar;
}  
//______________________________________________________________________________
//  
