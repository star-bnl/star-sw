//*-- Author : Renee Fatemi
// 
//
//September 21 2004 
//Maker collects information only used in simulations and outputs tree
//
#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"
#include "TTree.h"
#include "TLorentzVector.h"
#include "StChain.h"
     
#include "StJetMaker/StJetSimuUtil/StJetSimuTreeMaker.h"
#include "StJetMaker/StJetSimuUtil/StJetSimuTrigMaker.h"
#include "StJetMaker/StJetSimuUtil/StJetSimuWeightMaker.h"


ClassImp(StJetSimuTreeMaker)



StJetSimuTreeMaker::StJetSimuTreeMaker(const char *name, const char *outputName):StMaker(name){
  m = TString(outputName);
}

StJetSimuTreeMaker::~StJetSimuTreeMaker(){
}

Int_t StJetSimuTreeMaker::Init()
{
    
    
    //get Makers
    
  trigMaker=(StJetSimuTrigMaker *)GetMaker("SimuTrig");
  assert(trigMaker);
  weightMaker=(StJetSimuWeightMaker *)GetMaker("SimuWeight");
  assert(weightMaker);
  
  //root output file,tree & its branches   
  outfile = new TFile(m,"recreate");
  jTree  = new TTree("Event","Trigger, Pythia and Jet Data");
  jTree->Branch("evtid",&evtid,"evtID/I");
  jTree->Branch("pid",&pid,"pID/I");
  jTree->Branch("BHTmax",&BHTmax,"BHTmax/I");
  jTree->Branch("BJPmax",&BJPmax,"BJPmax/I");
  jTree->Branch("BJPsum",&BJPsum,"BJPsum/I");
  jTree->Branch("EHTmax",&EHTmax,"EHTmax/I");
  jTree->Branch("EJPmax",&EJPmax,"EJPmax/I");
  jTree->Branch("EJPsum",&EJPsum,"EJPsum/I");
  jTree->Branch("BJP",BJP,"BJP[12]/I");
  jTree->Branch("EJP",EJP,"EJP[6]/I");
  jTree->Branch("bbc",&bbc,"bbc/I");
  jTree->Branch("Badc",Badc,"Badc[48]/I");


  pid=0;
  evtid=0;

  bbc=0;
  for (int i=0;i<BBCadcNum;i++){
   Badc[i]=0;
  } 

  BHTmax=0;
  BJPmax=0;
  BJPsum=0;
  EHTmax=0;
  EJPmax=0;
  EJPsum=0;

  
   return StMaker::Init();
}



//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StJetSimuTreeMaker::Make(){

  //SubProcess ID from StMcEventMaker
  pid=weightMaker->pid;
  if (print) printf("Weight Maker PID=%d\n",pid);
  evtid=weightMaker->evtid;
  if (print) printf("Weight Maker evtid=%d\n",evtid);

  //BBC TRIGGER
  bbc = trigMaker->bbcTrig;
   if (print){
     cout << "BBC trig == " << bbc << endl;
     if (bbc==1) cout << "BBC trigger is true! " << endl;
     if (bbc==0) cout << "NO BBC trigger! " << endl;
   }
   for (int i=0;i<48;i++){
     Badc[i]= trigMaker->BBCadc[i];
     if (print) cout << "Here is the ADC == " << Badc[i] << endl;
   }

   
  //BEMC TRIGGER
  BHTmax = trigMaker->BHTmaxt;  
  BJPmax = trigMaker->BJPmaxt;
  BJPsum = trigMaker->BJPsumt;
  EHTmax = trigMaker->EHTmaxt;  
  EJPmax = trigMaker->EJPmaxt;
  EJPsum = trigMaker->EJPsumt;
  for (int i=0;i<6;i++){
    BJP[i] = trigMaker->jpBsum[i];
    EJP[i] = trigMaker->jpEsum[i];
    if (print) printf("BJP = %d, EJP = %d\n",BJP[i],EJP[i]);
  }
  if (print) printf("Max Ht in BEMC=%d, Max JP Sum =%d, Total ADC BEMC Sum = %d\n", BHTmax, BJPmax, BJPsum);
  
  jTree->Fill();     
  return kStOK;   
  }


Int_t StJetSimuTreeMaker::Finish()
{
  // jTree->Scan("pID");
  outfile->Write();
  outfile->Close();
  delete outfile;
  StMaker::Finish();
  return kStOK;
}









