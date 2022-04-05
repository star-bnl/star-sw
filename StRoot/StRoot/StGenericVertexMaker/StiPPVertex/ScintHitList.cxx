#include <string.h>
#include <assert.h>
#include <stdio.h>

#include <TH1.h>
#include <TObjArray.h>
#include <St_base/StMessMgr.h>

#include "StGenericVertexMaker/StiPPVertex/ScintHitList.h"

//==========================================================
//==========================================================
ScintHitList::ScintHitList(float Xphi0,float XdPhi,int mxPhi,
			   float Xeta0, float XdEta,int mxEta,
			   const char* name, float wm, float wv) {
  phi0=Xphi0;
  dPhi=XdPhi;
  nPhi=mxPhi;
  eta0=Xeta0;
  dEta=XdEta;
  nEta=mxEta;
  nBin=nEta*nPhi;  
  myName=name;
  Wmatch=wm;
  Wveto=wv;
  active= new int[nBin]; 
  fired=  new int[nBin]; 
  track=  new int[nBin]; 
  memset(h,0,sizeof(h));
  clear();
  gMessMgr->Message("","I") 
    <<"ScintHitList for "<<myName
    <<", use W_match="<< Wmatch<<" W_veto="<<Wveto
    <<" nEta="<< nEta<<" nPhi="<< nPhi<<" nBin="<< nBin
    <<endm;
}

//==========================================================
//==========================================================
void
ScintHitList::initRun(){
  memset(active,0,nBin*sizeof(int));
  nActive=0;
  clear();
  gMessMgr->Message("","D") 
    <<"Clear ScintHitList for "<<myName<<endm;
}

//==========================================================
//==========================================================
ScintHitList::~ScintHitList(){
  delete [] active ;
  delete [] fired ;
  delete [] track ;
}


//==========================================================
//==========================================================
void
ScintHitList::clear(){
  memset(fired,0,nBin*sizeof(int));
  memset(track,0,nBin*sizeof(int));
  nFired=nTrack=nMatch=0;
}

//==========================================================
//==========================================================
int 
ScintHitList::iPhiEta2bin(int iPhi,int iEta) {
  int  iBin=iPhi+nPhi*iEta;
  assert(iBin>=0);
  assert(iBin<nBin);
  return iBin;
}

//==========================================================
//==========================================================
int 
ScintHitList::getActive(int iBin){
  if(iBin<0 || iBin>=nBin) return -1;
  return active[iBin];
}

//==========================================================
//==========================================================
int 
ScintHitList::getFired(int iBin){
  if(iBin<0 || iBin>=nBin) return -1;
  return fired[iBin];
}

//==========================================================
//==========================================================
int 
ScintHitList::getTrack(int iBin){
  if(iBin<0 || iBin>=nBin) return -1;
  return track[iBin];
}

//==========================================================
//==========================================================
bool
ScintHitList::isMatched(int iBin){
  if(iBin<0 || iBin>=nBin) return false;
  if(active[iBin]>0 && fired[iBin]>0 && track[iBin]>0 ) return true;
  return false;
}

//==========================================================
//==========================================================
bool
ScintHitList::isVetoed(int iBin){
  if(iBin<0 || iBin>=nBin) return false;
  if(active[iBin]>0 && fired[iBin]==0 && track[iBin]>0 ) return true;
  return false;
}

//==========================================================
//==========================================================
float
ScintHitList:: getWeight(int iBin){
  const float Wdunno=1;
  if( getActive(iBin) <=0) return Wdunno;
  if(isMatched(iBin)) return Wmatch;  
  if(isVetoed(iBin)) return Wveto;
  return Wdunno;

}

//==========================================================
//==========================================================
void 
ScintHitList::iBin2iPhiEta(int iBin,int &iPhi,int &iEta) {
  assert(iBin>=0);
  assert(iBin<nBin);
  iPhi=iBin%nPhi;
  iEta=iBin/nPhi;
}

//==========================================================
//==========================================================
void 
ScintHitList::setActive(int iBin){
  assert(iBin>=0);
  assert(iBin<nBin);
  active[iBin]=1;
  nActive++;
}

//==========================================================
//==========================================================
void 
ScintHitList::setFired(int iBin){
  assert(iBin>=0);
  assert(iBin<nBin);
  if(active[iBin]<=0) return; // sensitive only to active pixels
  fired[iBin]=1;
  nFired++;
}

//==========================================================
//==========================================================
int
ScintHitList::phiBin(float phi){// phi is [0,2Pi]
  
  int iPhi=(int)((phi-phi0)/dPhi);
  if(iPhi<0) iPhi+=nPhi; // I'm a bit forgiving here,JB
  if(iPhi>=nPhi) iPhi-=nPhi;// I'm a bit forgiving here,JB
  assert(iPhi>=0);
  assert(iPhi<nPhi);
  return iPhi;
}

//==========================================================
//==========================================================
int 
ScintHitList:: addTrack(float eta, float phi){
  int iBin=-1;
  
  int iEta=etaBin(eta);
  int iPhi=phiBin(phi);
  if(iEta<0 || iPhi<0 || iEta>=nEta || iPhi>=nPhi) return iBin; // out of Eta range
  iBin=iPhiEta2bin(iPhi,iEta);
  track[iBin]++;
  nTrack++;
  //  printf(" %s-addTrack() eta=%.3f phi/deg=%.1f iBin=%d fired=%d iEta=%d iPhi=%d\n",myName.Data(),eta,phi/3.1416*180,iBin,fired[iBin],iEta,iPhi);
  if( fired[iBin]>0) { 
    nMatch++;
    if(h[6]) {// drop if histos not initialized
      float eps=eta - (eta0+iEta*dEta); //<<-use bin2EtaLeft(int iEta), should work for endcap
      // printf(" addTrack() eta=%.3f iEta=%d eps=%.3f \n",eta,iEta,eps);
      h[6]->Fill(eps);
      eps=phi - (phi0+iPhi*dPhi);
      // printf(" addTrack() phi=%.3f/rad iPhi=%d eps=%.3f \n",phi,iPhi,eps);
      h[7]->Fill(eps/3.1416*180);
    }
  }
 return iBin;
}


//==========================================================
//==========================================================
void
ScintHitList::print(int k){

  LOG_INFO<< Form("%sHitList: nActive=%d nFired=%d nTrack=%d nMatch=%d",myName.Data(),nActive, nFired,nTrack,nMatch)<<endm;
  LOG_DEBUG << Form("iBin iEta iPhi active:fired:track  (LEFT: phi/deg, eta)")<<endm;
  int i;
  int nb=0;
  for(i=0;i<nBin;i++){
    if(k==0 && fired[i]==0 && track[i]==0) continue;
    nb++;
    int iPhi,iEta;
    iBin2iPhiEta(i,iPhi,iEta);
    char mm=' ';
    if(fired[i] && track[i]) mm='*';
    float etaF=bin2EtaLeft(iEta);
    LOG_DEBUG << Form("%3d %3d %3d   %d:%d:%d %c  (%.1f, %.3f)",i,iEta,iPhi,active[i],fired[i],track[i],mm,phi0+iPhi*dPhi/3.1416*180., etaF )<<endm;
  }
  LOG_DEBUG << Form("--- %d printed bins",nb)<<endm;
}

//==========================================================
//==========================================================
void
ScintHitList::doHisto(){

  //  printf("fill ScintHitList histos for this event\n");
  h[0]->Fill(nFired);
  h[2]->Fill(nTrack);
  h[4]->Fill(nMatch);

  int i;
  for(i=0;i<nBin;i++){
    if(fired[i]) h[1]->Fill(i);
    if(track[i]) h[3]->Fill(i,track[i]); // could be more than one
    if(isMatched(i)) h[5]->Fill(i);
  }
}

//==========================================================
//==========================================================
void
ScintHitList:: initHisto (TObjArray*HList){
  const char *core=myName.Data();
  char *type[3]={(char *) "Fired",(char *) "Track",(char *) "Matched tracks"};
  char tt1[100],tt2[500];
 
  int it;
  for(it=0;it<3;it++){
    sprintf(tt1,"n%s%c",core,type[it][0]);
    sprintf(tt2,"%s hits n%s / eve",core,type[it]);
    int nb=70;
    if(it==1) nb=300;
    h[2*it]=new TH1F(tt1,tt2,nb,-0.5,nb-0.5);
    HList->Add(h[2*it]);
  }

  for(it=0;it<3;it++){
    sprintf(tt1,"%s%c",core,type[it][0]);
    sprintf(tt2,"%s : frequency  of %s / element",core,type[it]);
    h[2*it+1]=new TH1F(tt1,tt2,nBin,0.5,nBin+0.5);
    HList->Add(h[2*it+1]);
  }

  sprintf(tt1,"%sDeta",core);
  sprintf(tt2,"%s tracks matched to fired cell; delta eta ",core);
  h[6]=new TH1F(tt1,tt2,200,-2*dEta,2*dEta); // scint dependent
  HList->Add(h[6]);

  sprintf(tt1,"%sDphi",core);
  sprintf(tt2,"%s tracks matched to fired cell; delta phi/deg ",core);
  float xx=2*dPhi/3.1416*180;
  h[7]=new TH1F(tt1,tt2,200,-xx,xx); // scint dependent
  HList->Add(h[7]);

  // note, delEta monitoring histo is not working properly for Endcap,but decisions are fine 


}


