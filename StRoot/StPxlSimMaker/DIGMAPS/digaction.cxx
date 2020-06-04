///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//    DIGAction                                                                              //
//                                                                                           //
//    Classe containing the main action foreseen by the program                              //
//     (make the tree, plot, etc.).                                                          //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <digaction.h>

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
ClassImp(DIGAction)

DIGAction::DIGAction()  
{

}
//______________________________________________________________________________
//  
DIGAction::DIGAction(Char_t *Doit,Char_t *Model)
{
  strcpy(fDoit,Doit);
  strcpy(fModel,Model);
}
//______________________________________________________________________________
//  

DIGAction::~DIGAction() { // 
  // virtual destructor
  //
  
}
//______________________________________________________________________________
//  
void DIGAction::SetDoit(Char_t *Doit) {
  strcpy(fDoit,Doit);
}
//______________________________________________________________________________
//  
void DIGAction::SetModel(Char_t *Model) {
  strcpy(fModel,Model);
}
//______________________________________________________________________________
//  
void DIGAction::PrintInfo() {

  std::cout<<"-----DIGACTION infos-----"<<endl;
  std::cout<<" action"<<endl;
  printf("%s\n",fDoit);
  std::cout<<" model"<<endl;
  printf("%s\n",fModel);
}
//==============================================================================
