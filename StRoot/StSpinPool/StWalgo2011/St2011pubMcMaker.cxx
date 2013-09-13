// $Id: St2011pubMcMaker.cxx,v 1.3 2013/09/13 19:33:13 stevens4 Exp $
//
//*-- Author : Justin Stevens, IUCF
// 

#include "St2011WMaker.h"
#include "St2011pubMcMaker.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

//need these to get MC record
#include "tables/St_g2t_tpc_hit_Table.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"
#include "StMcEvent/StMcTrack.hh"

ClassImp(St2011pubMcMaker)

//_______________________________________________________
//_______________________________________________________
St2011pubMcMaker::St2011pubMcMaker(const char *name):StMaker(name){
  wMK=0;HList=0;
    
}


//_______________________________________________________
//_______________________________________________________
St2011pubMcMaker::~St2011pubMcMaker(){
  //
}


//_______________________________________________________
//_______________________________________________________
Int_t St2011pubMcMaker::Init(){
  assert(wMK);
  assert(HList);
  initHistos();
  return StMaker::Init();
}


//_______________________________________________________
//_______________________________________________________
Int_t 
St2011pubMcMaker::Make(){
  //printf("-----------in %s\n", GetName());
  //only get geant particle info for W MC
  if(wMK->isMC==350){
    if(doMCanalysis()){
      doWanalysis();
      doWefficiency();
    }
  }
  return kStOK;
}

//_______________________________________________________
//_______________________________________________________
void 
St2011pubMcMaker::doWanalysis(){
  //has access to whole W-algo-maker data via pointer 'wMK'
 
  // run through W cuts to fill other histos............
  for(uint iv=0;iv<wMK->wEve->vertex.size();iv++) {
    WeveVertex &V=wMK->wEve->vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error
      
      if(T.cluster.ET /T.nearTotET< wMK->par_nearTotEtFrac) continue; // too large nearET
      if(T.awayTotET> 30.) continue; // too large awayET , Jan
      //Full W cuts applied at this point

      //W info from pythia record
      hA[1]->Fill(mWP.Perp());
      hA[2]->Fill(mWP.z());
      
      hA[24]->Fill(mWP.Eta());
      hA[25]->Fill(T.hadronicRecoil.Eta());
      hA[26]->Fill(mWP.Perp(),T.hadronicRecoil.Eta());
      if(mWP.Eta()>0) {
	hA[27]->Fill(T.hadronicRecoil.Eta());
      }
      else {
	hA[28]->Fill(T.hadronicRecoil.Eta());
      }
      
      //hadronic recoil and correlations with W from pythia
      TVector3 hadronicPt(T.hadronicRecoil.X(),T.hadronicRecoil.Y(),0); //transverse momentum vector
      hA[3]->Fill(mWP.Perp()-hadronicPt.Perp());
      hA[4]->Fill(mWP.Perp(),hadronicPt.Perp());
      hA[5]->Fill(hadronicPt.Perp()); 

      float delPhi=mWP.DeltaPhi(-hadronicPt);
      hA[6]->Fill(mWP.Perp(),delPhi);
      hA[7]->Fill(mWP.Perp(),mWP.Perp()-hadronicPt.Perp());
      hA[8]->Fill(T.hadronicRecoil.Perp(),delPhi);
      
      //electron reco and from geant record
      hA[9]->Fill(mElectronP.Perp());
      hA[10]->Fill(T.cluster.ET);      
      hA[11]->Fill(mElectronP.Perp(),T.cluster.ET);
      hA[12]->Fill(mElectronP.Perp(),mElectronP.Perp()-T.cluster.ET);

      TVector3 electronPt(T.cluster.position.X(),T.cluster.position.Y(),0); //transvers momentum vector
      electronPt.SetMag(T.cluster.ET);
      
      //neutrino reco and from geant record
      TVector3 neutrinoPt=-1*(hadronicPt+electronPt);
      hA[13]->Fill(neutrinoPt.Perp());  
      hA[14]->Fill(mNeutrinoP.Perp());
      hA[15]->Fill(mNeutrinoP.Perp(),neutrinoPt.Perp());
      hA[16]->Fill(mNeutrinoP.Perp()-neutrinoPt.Perp());

      hA[17]->Fill(mNeutrinoP.Perp(),mElectronP.Perp());

      float recoDeltaPhi=neutrinoPt.DeltaPhi(electronPt);
      float geantDeltaPhi=mNeutrinoP.DeltaPhi(mElectronP);

      hA[18]->Fill(geantDeltaPhi,recoDeltaPhi);
      hA[19]->Fill(cos(geantDeltaPhi)-cos(recoDeltaPhi));

      float Mt=sqrt(2*T.cluster.ET*neutrinoPt.Perp()*(1-cos(recoDeltaPhi)));  //real data
      float gMt=sqrt(2*mElectronP.Perp()*mNeutrinoP.Perp()*(1-cos(geantDeltaPhi)));
      
      hA[20]->Fill(Mt);  
      hA[21]->Fill(gMt);

      hA[22]->Fill(Mt,T.cluster.ET);
      hA[23]->Fill(gMt-Mt);

      //Kinematics 
      //reconstruct W pL from reconstructed quantities
      float trueWpL=mWP.z();
      float eleTheta=T.primP.Theta();
      float ratioE=T.cluster.energy/40.0;  
      float pLRecoPlus=80.0*(ratioE)*((cos(eleTheta))+sqrt(cos(eleTheta)*cos(eleTheta)+sin(eleTheta)*sin(eleTheta)*(1-ratioE*ratioE)))/(ratioE*ratioE*sin(eleTheta)*sin(eleTheta));//+ sqrt solution
      float pLRecoMinus=80.0*(ratioE)*((cos(eleTheta))-sqrt(cos(eleTheta)*cos(eleTheta)+sin(eleTheta)*sin(eleTheta)*(1-ratioE*ratioE)))/(ratioE*ratioE*sin(eleTheta)*sin(eleTheta));//- sqrt solution
      hA[29]->Fill(pLRecoPlus);
      hA[30]->Fill(trueWpL-pLRecoPlus);
      hA[31]->Fill(trueWpL,pLRecoPlus);
      
      hA[32]->Fill(pLRecoMinus);
      hA[33]->Fill(trueWpL-pLRecoMinus);
      hA[34]->Fill(trueWpL,pLRecoMinus);

      const StMuTrack *prTr=T.prMuTrack; assert(prTr);
      float p_chrg=prTr->charge();
      if(p_chrg > 0) continue;

      float eleEta=T.primP.Eta();
      //sort 2 solutions by electron eta
      if(eleEta<-0.8) {
	hA[35]->Fill(trueWpL,pLRecoMinus);
	hA[37]->Fill(trueWpL,pLRecoPlus);
      }
      else if(eleEta>0.8) {
	hA[36]->Fill(trueWpL,pLRecoMinus);
	hA[38]->Fill(trueWpL,pLRecoPlus);
      }
      
      if(T.cluster.ET < 30) continue; //only W's we find in data
      //Correlate W pL with electron E in 3 electron eta ranges
      if(eleEta < -0.8) {
	hA[39]->Fill(mWP.z(),T.cluster.energy);
	hA[42]->Fill(T.cluster.energy);
      }
      if(eleEta > 0.8){
	hA[40]->Fill(mWP.z(),T.cluster.energy);
	hA[43]->Fill(T.cluster.energy);
      }      
      if(eleEta > -0.1 && eleEta < 0.1) {
	hA[41]->Fill(mWP.z(),T.cluster.energy);
	hA[44]->Fill(T.cluster.energy);	
      }

    }
  }
}


//_______________________________________________________
//_______________________________________________________
void 
St2011pubMcMaker::doWefficiency(){

  //only count leptons in our eta range
  if(fabs(mElectronP.Eta()) > 1.) return;
  //ele has |eta| < 1
  hA[50]->Fill(mElectronP.Perp());
  hA[54]->Fill(mElectronP.Eta());
  hA[58]->Fill(mVertex.Z());
  hA[62]->Fill(mElectronP.Phi());

  hA[68]->Fill(mElectronP.Perp()); //forJoe

  //plot for Scott
  TVector3 detEle; //where lepton would hit BEMC
  float Rcylinder= wMK->mBtowGeom->Radius();
  detEle.SetPtEtaPhi(Rcylinder,mElectronP.Eta(),mElectronP.Phi());
  detEle.SetZ(detEle.Z()+mVertex.Z());
  hA[66]->Fill(detEle.Eta(),mElectronP.Perp());

  //trigger efficiency
  if(!wMK->wEve->l2bitET) return;
  //good trig
  hA[51]->Fill(mElectronP.Perp());
  hA[55]->Fill(mElectronP.Eta());
  hA[59]->Fill(mVertex.Z());
  hA[63]->Fill(mElectronP.Phi());
  hA[69]->Fill(mElectronP.Perp());//forJoe

  hA[67]->Fill(detEle.Eta(),mElectronP.Perp());

  //vertex efficiency
  if(wMK->wEve->vertex.size()<=0) return;
  //vertex rank>0 and |z|<100
  hA[52]->Fill(mElectronP.Perp()); 
  hA[56]->Fill(mElectronP.Eta());
  hA[60]->Fill(mVertex.Z());
  hA[64]->Fill(mElectronP.Phi());

  hA[70]->Fill(mElectronP.Perp());//forJoe

  //float diff=wMK->wEve->vertex[0].z - mVertex.Z();
  //cout<<"diff="<<diff<<endl;

  //reco efficiency
  for(uint iv=0;iv<wMK->wEve->vertex.size();iv++) {
    WeveVertex &V=wMK->wEve->vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error
      
      if(T.cluster.ET/T.nearTotET< wMK->par_nearTotEtFrac) 
	continue; // too large nearET
      if(T.ptBalance.Perp() < wMK->par_ptBalance || T.awayTotET > 30.) //Jan
	continue;
      
      //pass all W cuts 
      hA[53]->Fill(mElectronP.Perp());
      hA[57]->Fill(mElectronP.Eta());
      hA[61]->Fill(mVertex.Z());
      hA[65]->Fill(mElectronP.Phi());

      hA[71]->Fill(mElectronP.Perp());//forJoe
    }
  }

}


//________________________________________________
//________________________________________________
bool
St2011pubMcMaker::doMCanalysis(){
  StMcEvent* mMcEvent = 0;  
  mMcEvent = (StMcEvent*) StMaker::GetChain()->GetDataSet("StMcEvent");
  assert(mMcEvent);

  //initialize momentum vectors
  StThreeVectorF pW;        float eW;
  StThreeVectorF pNeutrino; //float eNeutrino;
  StThreeVectorF pElectron; //float eElectron;

  StMcVertex *V=mMcEvent->primaryVertex(); 
  mVertex=TVector3(V->position().x(),V->position().y(),V->position().z());
  
  uint i=1;
  int found=0;
  while(found<2 && i<mMcEvent->tracks().size()){//loop tracks
    StMcTrack* mcTrack = mMcEvent->tracks()[i];
    int pdgId=mcTrack->pdgId();
    //float pt=mcTrack->pt();
    //LOG_INFO<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;
    if(pdgId==11 || pdgId==-11){ //select e+ and e-
      if(abs(mcTrack->parent()->pdgId()) == 24 ){ 
	pElectron=mcTrack->momentum();
	//LOG_INFO<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;
	pW=mcTrack->parent()->momentum();
	eW=mcTrack->parent()->energy();
	//LOG_INFO<<"pdgId "<<mcTrack->parent()->pdgId()<<" pt "<<mcTrack->parent()->pt()<<" pz "<<mcTrack->parent()->momentum().z()<<endm;
	found++;
      }
    }
    if(pdgId==12 || pdgId==-12){ //select neutrino
      if(abs(mcTrack->parent()->pdgId()) == 24 ){ 
	pNeutrino=mcTrack->momentum();
	//LOG_INFO<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;
	pW=mcTrack->parent()->momentum();
	eW=mcTrack->parent()->energy();
	//LOG_INFO<<"pdgId "<<mcTrack->parent()->pdgId()<<" pt "<<mcTrack->parent()->pt()<<" pz "<<mcTrack->parent()->momentum().z()<<endm;
	found++;
      }
    }
    i++;
  }

  if(found!=2) return false;
 
  mWP=TVector3(pW.x(),pW.y(),pW.z());
  mNeutrinoP=TVector3(pNeutrino.x(),pNeutrino.y(),pNeutrino.z());
  mElectronP=TVector3(pElectron.x(),pElectron.y(),pElectron.z());
  TVector3 diff=mWP-mNeutrinoP-mElectronP;
  if(diff.Mag() > 0.0001) //should get exactly right
    LOG_INFO<<"\n \n W+e+nu vector sum ="<<diff.Mag()<<endm;
  if(mElectronP.Mag() < 0.0001)
    LOG_INFO<<"\n \n no lepton track ="<<endm;

  //calculate x1 and x2 from W rapidity
  float rapW = 0.5*log((eW+mWP.Z())/(eW-mWP.Z()));
  float mw2sqs=80.4/500.;
  float x1=mw2sqs*exp(rapW);
  float x2=mw2sqs*exp(-rapW);

  hA[72]->Fill(rapW);
  if(fabs(mElectronP.Eta())<1){ //require midrapidity e
    hA[73]->Fill(x1);
    hA[74]->Fill(x2);
    hA[75]->Fill(x1,x2);
    hA[76]->Fill(x1-x2);
  }

  return true;

}

// $Log: St2011pubMcMaker.cxx,v $
// Revision 1.3  2013/09/13 19:33:13  stevens4
// Updates to code for combined 2011+2012 result presented to spin PWG 9.12.13
//
// Revision 1.2  2013/07/03 16:53:07  stevens4
// Update for efficiency studies with embedding
//
// Revision 1.1  2011/02/10 20:33:25  balewski
// start
//
