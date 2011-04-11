//*-- Author :Weihong He
// 
// $Id: StMcOutputMaker.cxx,v 1.3 2011/04/11 19:35:41 fisyak Exp $
#include <TH2.h>

#include "StMcOutputMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"
#include "StMcEvent/StMcTrack.hh"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
//MuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include "StarClassLibrary/StParticleDefinition.hh"



ClassImp(StMcOutputMaker)


StMcOutputMaker::StMcOutputMaker(const char *name):StMaker(name){
  memset(hA,0,sizeof(hA));
  HList=0;
  geomE= new EEmcGeomSimple();

}


StMcOutputMaker::~StMcOutputMaker(){
  //
}


//_____________________________________________________________________________
//
Int_t 
StMcOutputMaker::Init(){


  memset(hA,0,sizeof(hA));
  hA[0]=new TH2F("gEEmcEtaPhi", "Generated pi0 track; phi / deg;EEmc Eta",360,-180.,180,20,1.,2.);
  //hA[1]=new TH2F("rEtaPhi", "Recon pi0; track eta; phi / deg",20,0.5,2.5,20,0,90);
  hA[1]=new TH1F("gPt", "Generated pi0 Pt ",50,0.,25.);
  hA[2]=new TH1F("gEta", "Generated pi0 eta ",50,0.,2.5);
  hA[3]=new TH1F("gEnergy","generated Mc pi0 energy",80,0.,80.);
  hA[4]=new TH1F("gZgg","generated Mc pi0 energy sharing",100,0.,1.);
  hA[5]=new TH1F("gZgg01","generated Mc pi0 energy sharing[5,10]",100,0.,1.);
  hA[6]=new TH1F("gZgg02","generated Mc pi0 energy sharing[10,15]",100,0.,1.);
  hA[7]=new TH1F("gZgg03","generated Mc pi0 energy sharing[15,20]",100,0.,1.);
  hA[8]=new TH1F("gZgg04","generated Mc pi0 energy sharing[20,30]",100,0.,1.);
  hA[9]=new TH1F("gZgg05","generated Mc pi0 energy sharing[30,60]",100,0.,1.);
  hA[10]=new TH1F("gPhi", "Generated pi0 Phi ",360,-180.,180.);
  hA[11]=new TH1F("gEEmcEta", "Generated pi0 eta in EEMC ",50,0.,2.5);
  hA[12]=new TH1F("DeltaEta", "genEta - EEmcEta ",40,-2.,2.);
  hA[13]=new TH1F("gZvert", "generated Z vertex ",300,-150.,150.);
  //hA[3]=new TH1F("rEta", "Recon pi0 eta ",40,0.5,2.5);
  //hA[4]=new TH1F("rNP", "nFitPoints,  Recon pi0; nFitP ",50,-0.5,49.5);
  //hA[5]=new TH1F("rrpt", "Pt resolution, ptGen=6 GeV ; gen-reco/gen",100,-0.5,0.5);
 
  assert(HList);
  int i;
  for(i=0;i<mxHa;i++) if(hA[i]) HList->Add(hA[i]);

   return StMaker::Init();
}

//________________________________________________
//________________________________________________
void
StMcOutputMaker::Clear(const Option_t*){
  gTr.clear();
  geemcEta.clear();
  genZZ.clear();
  genXX.clear();
  genYY.clear();
}

//________________________________________________
//________________________________________________
Int_t 
StMcOutputMaker::Make(){
  //
  //PrintInfo();
  //
  int fflag=0;
  float hlow=270.0*tan(15.0*3.14159265/180.0);
  float hhigh=270.0*tan(40.0*3.14159265/180.0);
  //std::cout<<"hlow="<<hlow<<"hhigh="<<hhigh<<std::endl;


  probTr= StLorentzVectorF();
  
  // **************************************
  // get pointer to mcEventMaker, Event *
  // **************************************
  StMcEvent* mMcEvent = 0;  //!
  mMcEvent = (StMcEvent*) StMaker::GetChain()->GetDataSet("StMcEvent"); 
  assert(mMcEvent); 

  //#if 0
  //StMcVertex*  PV=  mMcEvent->       primaryVertex();
  //printf("%s:: nPrimV=%p\n", GetName(), (void*)V);
  //assert(PV);
  //int nPD=PV-> numberOfDaughters();
  //#endif 
  //printf("n prim Doughters=%d\n",nPD);
  //all vertices
  const StSPtrVecMcVertex &VL=mMcEvent->vertices();
  //printf("%s::nVert=%d\n",GetName(),VL.size());
  //  int ipr=1;
  UInt_t i;
  zgg=0;
  for( i=0;i<VL.size();i++) { 
    StMcVertex* V=VL[i];
    
    // Rxy vx z vertex to remove fake electrons
    float x=V->position().x();
    float y=V->position().y();
    float zz=V->position().z();
    float Rxy=TMath::Sqrt(x*x+y*y);
    
    //printf("vx=%f,vy=%f,vz=%f Rxy=%f\n",x,y,z,Rxy);
    //cout<<*V<<endl<<endl;
    int nD=V-> numberOfDaughters();
    //cout <<Form("#daughters=%d\n",nD)<<endl;
    for(int it=0; it<nD;it++ ) {
      StMcTrack* tr=V->daughter(it);
      int gid=tr->geantId();
      if(gid!=7)
	{continue;}
      float hHeight=0.0;
      StLorentzVectorF p4=tr->fourMomentum();
      float psRap=tr->pseudoRapidity();
      if( psRap<0 || psRap>3.) continue; // reject sputnik-muons
      //  cout<<*tr<<endl;
      probTr=p4;
      float ppt=p4.perp();
      if(ppt<=3.0) continue;
      float ppz=p4.pz();
      hHeight=ppt*(270.0-zz)/ppz+Rxy;
      
      if(hHeight<=hlow || hHeight >=hhigh) continue;
      fflag++;
      float etatheta=TMath::ATan(hHeight/270.0);
      //printf("theta=%f etatheta=%f\n",p4.theta(), etatheta);
      float mideta=tan(etatheta/2.0);
      float eemceta=-log(mideta);
      //printf("geta=%f eemceta=%f zv=%f\n",tr->pseudoRapidity(),eemceta,zz);
      //printf(" GEANT prob: gid=%d  E/GeV=%.1f theta/deg=%.1f phi/deg=%.1f eta=%.2f\n",gid,p4.e(),p4.theta()/3.1416*180,p4.phi()/3.1416*180 ,tr->pseudoRapidity());
      hA[0]->Fill(p4.phi()/3.1416*180,eemceta);
      hA[2]->Fill(tr->pseudoRapidity());
      hA[10]->Fill(p4.phi()/3.1416*180);
      hA[1]->Fill(p4.perp());
      hA[3]->Fill(p4.e());
      hA[11]->Fill(eemceta);
      hA[12]->Fill(tr->pseudoRapidity()-eemceta);
      hA[13]->Fill(zz);
      //printf("eta=%f eta2=%f\n",p4.pseudoRapidity(),tr->pseudoRapidity());
      genE=0;
      genEta=0;
      genPhi=0;
      if(p4.e()>=0)
	{
	  genEta=tr->pseudoRapidity();
	  genE=p4.e();
	  genPhi=p4.phi()/3.1416*180;
	}
      gTr.push_back(tr);
      geemcEta.push_back(eemceta);
      genZZ.push_back(zz);
      genXX.push_back(x);
      genYY.push_back(y);
      //printf("zvertex=%f pt=%f pz=%f height=%f size=%d\n",zz,ppt,ppz,hHeight,gTr.size());
    }
    if(nD==2)
    {
      StMcTrack* tr1=V->daughter(0);
      int geid1=tr1->geantId();
      StMcTrack* tr2=V->daughter(1);
      int geid2=tr2->geantId();
      float Etot=0,Edif=0;
      if((geid1==1)&&(geid2==1))
	{
	  for(int it=0; it<nD;it++ ) { // primary tracks
	    StMcTrack* tr=V->daughter(it);
	    //int geid=tr->geantId();
	    //float phi=tr->momentum().phi(); 
	    //float eta=tr->pseudoRapidity();              
	    //float pt=tr->pt(); 
	    float ener=tr->fourMomentum().e();
	    //printf("geid=%d eta=%f phi=%f pt=%f e=%f\n",geid,eta,phi,pt,ener);
	    Etot+=ener;
	    Edif=ener-Edif;
	  }
	  zgg=fabs(Edif)/Etot;
	  //printf("zgg=%f\n",zgg);
	  hA[4]->Fill(zgg);
	  if(Etot>=5.0 && Etot<=10.0)
	    {
	      hA[5]->Fill(zgg);
	    }
	  if(Etot>=10.0 && Etot<=15.0)
	    {
	      hA[6]->Fill(zgg);
	    }
	  if(Etot>=15.0 && Etot<=20.0)
	    {
	      hA[7]->Fill(zgg);
	    }
	  if(Etot>=20.0 && Etot<=30.0)
	    {
	      hA[8]->Fill(zgg);
	    }
	  if(Etot>=30.0 && Etot<=60.0)
	    {
	      hA[9]->Fill(zgg);
	    }
	}
    }
  }
      //  printf("x=%f,y=%f,z=%f,Rxy=%f\n",x,y,z,Rxy);  

  //printf("found %d pi0\n",gTr.size());
  return kStOK;
}








