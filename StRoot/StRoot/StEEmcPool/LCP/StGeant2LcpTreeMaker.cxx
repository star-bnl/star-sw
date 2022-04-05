// *-- Author : Jan Balewski
// 
// $Id: StGeant2LcpTreeMaker.cxx,v 1.4 2007/07/12 19:24:55 fisyak Exp $

#include <math.h>
#include <TFile.h>
#include <TH2.h>

#include "StGeant2LcpTreeMaker.h"

#include "TChain.h"
#include "TClonesArray.h"

#include "StEventInfo.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "TMath.h"
ClassImp(StGeant2LcpTreeMaker)


//________________________________________________
//________________________________________________
StGeant2LcpTreeMaker::StGeant2LcpTreeMaker(const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);

  runID=101;// default not initialized value 
  treeName="./";
  SetMaxEta(1.0);
  SetMinPt(0.4); // GeV/c
}


//___________________ _____________________________
//________________________________________________
StGeant2LcpTreeMaker::~StGeant2LcpTreeMaker(){
  if(tree) tree->Print();
  
  // Save all objects in this file
  hfile->Write();
  
}


//________________________________________________
//________________________________________________
Int_t StGeant2LcpTreeMaker::Init(){

 return StMaker::Init();
}
 

//________________________________________________
//________________________________________________
void StGeant2LcpTreeMaker::clearLCP(){
  lcp_eta=lcp_phi=lcp_pt=lcp_e=0;
  lcp_idhep=0;
}
 
//________________________________________________
//________________________________________________
void StGeant2LcpTreeMaker::InitRunFromMake  (int runNumber){
  if(runID==runNumber)  return ;
  if(runID==888999)  return ; // special run number
  

  if(runID !=101) {
    printf("\n\n FATAL %s::InitRunFromMake() runID=%d changed to %d, (not implemented),STOP \n",GetName(),runID,runNumber);
    assert(runID==runNumber);
  }
  
  runID=runNumber;
  float Pi=3.14159;
  treeName+="/G";
  treeName+=runID;
  treeName+=".tree.root";

  printf("%s::Init() \n   runID=%d Setup output tree & histo to '%s' , CUTS:  maxEta=%.2f minPt/GeV/c=%.2f\n",GetName(),runID,treeName.Data(),C_maxEta,C_minPt);

  hfile=new TFile(treeName,"RECREATE"," histograms & trees with LCP ");
  assert(hfile->IsOpen());
  
  // Create some histograms and a profile histogram
  h[0]=0;
  h[1]=0;
  h[2]=0;
  h[3]=0;
  h[4]=0;
  h[5]   = new TH1F("nPKpi","No. of stable p,pbar,K+/-,pi+/- with |eta|<1 and pT>0.4",50,-0.5,49.5);
  h[6]   =new TH1F("eta","eta of LCP",100,-2.,2.);

  h[7]   = new TH1F("phi"," phi/rad  of LCP", 240,-Pi,Pi);
  h[8]   = new TH1F("pT","pT/GeV of LCP",100,0,10.);
  h[9]   = 0;
  h[10]   = 0;
  

  if (runID>100) {   
    // Create tree
    tree = new TTree("G-LCP","LCP selected from Geant data");
    tree->Branch("id",   &eve_id,"id/I");
    tree->Branch("sub",   &eve_sub,"sub/I");
    tree->Branch("nPKPi",&eve_nPKPi,"nPKPi/I");    
    tree->Branch("pt",&lcp_pt ,"pt/F");
    tree->Branch("phi",&lcp_phi ,"phi/F");
    tree->Branch("eta",&lcp_eta ,"eta/F");
    tree->Branch("e",   &lcp_e ,"e/F");
  } else {
    printf(" %s::Init(RunNumber=%d) WARN : TTRee NOT created\n",GetName(),runID);
    tree=0;
  }
return;
}  

//________________________________________________
//________________________________________________
Int_t StGeant2LcpTreeMaker::Make(){
  
  static int kEve=0;
  kEve++;
  clearLCP();
  
  printf("%s::Make() is called ..........\n",GetName());
  
  StMuDst* dst = mMuDstMaker->muDst();
    
  StMuEvent* muEve = dst->event();
  StEventInfo &info=muEve->eventInfo();

  if(runID>100) InitRunFromMake(info.runId());

  printf(" eve ID=%d\n",info.id());
    
 
  // Access to geant-tables .......................
  
  St_DataSet* Event = GetDataSet("geant");
  //Event->ls(3);
  St_DataSetIter geantDstI(Event);
  
  // Event generator info ........................
  St_g2t_event *Pg2t_event=(St_g2t_event *) geantDstI("g2t_event");
  
  g2t_event_st *g2t_event1=Pg2t_event->GetTable();
  printf("nr=%d \n",(int)Pg2t_event->GetNRows());
  int k1=	g2t_event1->eg_label;
  int k2=	g2t_event1->n_event;
  int k3=	g2t_event1->subprocess_id;
  
  printf("eg_label=%d n_event=%d subprocess_id=%d\n", k1,k2,k3);
  
  assert(info.id()==g2t_event1->n_event);
  eve_id=info.id();
  eve_sub=g2t_event1->subprocess_id;

  // This is an example to access the particle table directly.
  St_particle    *particleTabPtr    =  (St_particle    *) geantDstI("particle");
  
  findGeantLcp( particleTabPtr);
  
  if(tree)tree->Fill();
  
  //if(kEve%500==0)
    printf("#%s::Make(%d) nPKPi=%d  lcp: pt=%f idHep=%d \n",GetName(),kEve,eve_nPKPi, lcp_pt,lcp_idhep);
  
  
  return kStOK;
}


//================================================
//================================================
void StGeant2LcpTreeMaker::printTrack( particle_st* part) {
  cout << "   ist = " << part->isthep;
  cout << "   id = " << part->idhep << endl;
  cout << "   px = " << part->phep[0];
  cout << "   py = " << part->phep[1];
  cout << "   pz = " << part->phep[2] << endl;
  cout << "   e  = " << part->phep[3];
  cout << "   m  = " << part->phep[4] << endl;
  //cout << "   moth1  = " << part->jmohep[0] << endl;
  //cout << "   moth2  = " << part->jmohep[1] << endl;
    
}

//================================================
//================================================
particle_st* StGeant2LcpTreeMaker::findGeantLcp( St_particle    *tab) {
  assert(tab);
  particle_st* part = tab->GetTable();
  printf("tab dim=%d\n", (int)tab->GetNRows());
  int i;
  float maxPt=0.4;
  float maxEta=1.;
  float etaI=99;
  int iMax=-1;
  int ns=0, np=0,n2=0,n3=0,npkpi=0;
  for(i=0;i<tab->GetNRows();i++) {
    if(part[i].isthep!=1) continue;
    ns++;
    int id=part[i].idhep;
    int idM=id>0 ? id : -id ;
    if(idM!=211 && idM!=321 && idM!=2212) continue;
    // accept only p,pbar, pi+/-, K+/-
    np++;
    
    float px=part[i].phep[0];
    float py=part[i].phep[1];
    float pz=part[i].phep[2];

    float pt=sqrt(px*px+py*py);
    if(pt<0.001) continue; // to allow for math
    float eta= TMath::ASinH(pz/pt);

#if 0
    {
      float phi=atan2(part[i].phep[1],part[i].phep[0]);
      printf("i=%d pT=%f phi/deg=%f eta=%f pz=%f idHep=%d\n",i,pt,phi/3.1416*180,eta,pz,id);
    }
#endif
    //printf("px=%f py=%f pt=%f\n",px,py,pt);
    if(pt>0.4 && fabs(eta)<1) npkpi++;
    if(pt<maxPt) continue;
    n2++;

    if(fabs(eta)>maxEta) continue;
    // track is in eta range

    n3++;
    maxPt=pt;
    etaI=eta;
    iMax=i;
  }

  printf("#total %3d stable particles, np=%3d n2=%3d n3=%3d maxPt=%f iMax=%d eta=%f npkpi=%d\n",ns,np,n2,n3,maxPt,iMax,etaI,npkpi);

  eve_nPKPi=npkpi;
  h[5]->Fill( eve_nPKPi);

  if(iMax>=0) {
    printTrack(part+iMax);
    lcp_eta=etaI;
    lcp_phi=atan2(part[iMax].phep[1],part[iMax].phep[0]);
    lcp_pt=maxPt;
    lcp_idhep=part[iMax].idhep;
    lcp_e=part[iMax].phep[3];
    h[6]->Fill(lcp_eta);
    h[7]->Fill(lcp_phi);
    h[8]->Fill(lcp_pt);
    
    return part+iMax;
  }

  return 0;

}



// $Log: StGeant2LcpTreeMaker.cxx,v $
// Revision 1.4  2007/07/12 19:24:55  fisyak
// Add includes for TMath for ROOT 5.16
//
// Revision 1.3  2004/02/24 21:33:23  balewski
// *** empty log message ***
//
// Revision 1.2  2004/01/26 22:49:37  perev
// WarnOff
//
// Revision 1.1  2004/01/06 17:25:26  balewski
// get LCP from Geant info
//
