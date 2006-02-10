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
  jTree->Branch("bbc",&bbc,"bbc/I");
  jTree->Branch("Badc",Badc,"Badc[48]/I");
  jTree->Branch("s",&s,"s/F");
  jTree->Branch("t",&t,"t/F");
  jTree->Branch("u",&u,"u/F");
  jTree->Branch("hard_p",&hard_p,"hard_p/F");
  jTree->Branch("cos_th",&cos_th,"cos_th/F");
  jTree->Branch("x1",&x1,"x1/F");
  jTree->Branch("x2",&x2,"x2/F");
  jTree->Branch("Alex_ht_Et",&Alex_ht_Et,"Alex_ht_Et/F");
  jTree->Branch("Alex_ht_DSM",&Alex_ht_DSM,"Alex_ht_DSM/I");
  jTree->Branch("Alex_ht_id",&Alex_ht_id,"Alex_ht_id/I");
  jTree->Branch("JP1_2004",&JP1_2004,"JP1_2004/I");
  jTree->Branch("JP1_2004_Patch",&JP1_2004_Patch,"JP1_2004_Patch/I");
  jTree->Branch("JP1_2004_DSM",&JP1_2004_DSM,"JP1_2004/I");
  jTree->Branch("HT1_2004",&HT1_2004,"HT1_2004/I");
  jTree->Branch("HT1_2004_Tow",&HT1_2004_Tow,"HT1_2004_Tow/I");
  jTree->Branch("HT1_2004_DSM",&HT1_2004_DSM,"HT1_2004_DSM/I");
  jTree->Branch("partonic_all",&partonic_all,"partonic_all/D");
  jTree->Branch("df1",&df1,"df1/D");
  jTree->Branch("df2",&df2,"df2/D");
  jTree->Branch("f1",&f1,"f1/D");
  jTree->Branch("f2",&f2,"f2/D");
  jTree->Branch("Q2",&Q2,"Q2/D");
  jTree->Branch("weight",&weight,"weight/D");
  jTree->Branch("flavor1",&flavor1,"flavor1/I");
  jTree->Branch("flavor2",&flavor2,"flavor2/I");
  jTree->Branch("flavor3",&flavor3,"flavor3/I");
  jTree->Branch("flavor4",&flavor4,"flavor4/I");

  pid=0;
  evtid=0;
  s=0;
  t=0;
  u=0;
  hard_p=0;
  cos_th=0;
  x1=0;
  x2=0;
  weight=0;
  df1=0;
  df2=0;
  f1=0;
  f2=0;
  Q2=0;
  partonic_all=0;
  flavor1=0;
  flavor2=0;
  flavor3=0;
  flavor4=0;

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

  Alex_ht_DSM=0;
  Alex_ht_Et=0;
  Alex_ht_id=0;

  JP1_2004=-1;
  JP1_2004_Patch=-1;
  JP1_2004_DSM=-1;

  HT1_2004=-1;
  HT1_2004_Tow=-1;
  HT1_2004_DSM=-1;

   return StMaker::Init();
}



//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StJetSimuTreeMaker::Make(){
  
  
  pid=weightMaker->pid;
  evtid=weightMaker->evtid;
  s=weightMaker->s;
  t=weightMaker->t;
  u=weightMaker->u;
  hard_p=weightMaker->hard_p;
  cos_th=weightMaker->cos_th;
  x1=weightMaker->x1;
  x2=weightMaker->x2;
  df1=weightMaker->df1;
  df2=weightMaker->df2;
  f1=weightMaker->f1;
  f2=weightMaker->f2;
  Q2=weightMaker->Q2;
  flavor1=weightMaker->flavor1;
  flavor2=weightMaker->flavor2;
  flavor3=weightMaker->flavor3;
  flavor4=weightMaker->flavor4;
  partonic_all=weightMaker->partonic_all;
  weight=weightMaker->weight;
  

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
  
  Alex_ht_Et=trigMaker->Alex_ht_Et;
  Alex_ht_id=trigMaker->Alex_ht_id;
  Alex_ht_DSM=trigMaker->Alex_ht_DSM;
  /*** Uncomment once you have cvs co StEmcTriggerMaker and corresponding code in 
       StJetSimuTriggerMaker ***/
  //JP1_2004=trigMaker->JP1_2004_evt;
  //JP1_2004_Patch=trigMaker->JP1_2004_id;
  //JP1_2004_DSM=trigMaker->JP1_2004_dsm;
  //HT1_2004=trigMaker->HT1_2004_evt;
  //HT1_2004_Tow=trigMaker->HT1_2004_id;
  //HT1_2004_DSM=trigMaker->HT1_2004_dsm;
  
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









