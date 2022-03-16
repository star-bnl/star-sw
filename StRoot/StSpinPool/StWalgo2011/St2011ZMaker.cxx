// $Id: St2011ZMaker.cxx,v 1.13 2016/01/08 02:08:49 jlzhang Exp $
//
//*-- Author : Ross Corliss, MIT
//  changes Jan Balewski, MIT
//  changes Justin Stevens, MIT

#include <TMath.h>

#include "St2011WMaker.h"
#include "WeventDisplay.h"
#include "St2011ZMaker.h"

ClassImp(St2011ZMaker)
const float PI=TMath::Pi();

//_____________________________________________________________________________
//
St2011ZMaker::St2011ZMaker(const char *name):StMaker(name){
  wMK=0;muMK=0;HList=0;

}


//_____________________________________________________________________________
//
Int_t St2011ZMaker::Init(){
  assert(wMK);
  assert(HList);
  initHistos();
  return StMaker::Init();
}


//_____________________________________________________________________________
//
Int_t St2011ZMaker::InitRun  (int runumber){
  LOG_INFO<<Form("::InitRun(%d) done, Z-algo params: nearTotEtFrac=%.2f,  clusterEt=%.1f GeV, delPhi12>%.2f rad, Zmass in[%.1f,%.1f]\n",
		 runumber, par_nearTotEtFracZ,par_clusterEtZ,par_delPhi12,par_minMassZ,par_maxMassZ)<<endm;
  return 0;
}

//_____________________________________________________________________________
//
Int_t St2011ZMaker::FinishRun  (int runnumber){
return 0;
}

//_____________________________________________________________________________
//
Int_t 
St2011ZMaker::Make(){

  //fill various histograms and arrays with event data
  find_Z_boson();
  findEndcap_Z_boson();

  return kStOK;
}

//============================
void
St2011ZMaker::printJan(WeveEleTrack *T) {
  int ibp=kBTow;
  WevePointTower poiTw=T->pointTower;
  WeveCluster cl=T->cluster;
  int id= poiTw.id;
  float adc= wMK->wEve->bemc.adcTile[ibp][id-1];
  float frac= adc/4096*60 /cl.ET;
  printf("Ztower Q=%d pointTw: id=%d ADC=%.0f  2x2ET=%.1f frac=%.2f\n",T->prMuTrack->charge(),id,adc,cl.ET,frac);
}


//_____________________________________________________________________________
//
void
St2011ZMaker::findEndcap_Z_boson(){
  Wevent2011 *wEve=wMK->wEve;
  // printf("========= findEndcap_Z_boson() \n");
  
  hA[50]->Fill("inp",1.); 

  // search for  Zs ............
  for(uint iv=0;iv<wEve->vertex.size();iv++) {
    hA[50]->Fill("vert",1.);
    WeveVertex &V=wEve->vertex[iv];

    //first loop over good barrel tracks
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &TB=V.eleTrack[it];
      if(TB.pointTower.id<=0) continue; //skip endcap towers
      if(TB.isMatch2Cl==false) continue;
      assert(TB.cluster.nTower>0); // internal logical error
      assert(TB.nearTotET>0); // internal logical error
   
      //place cuts on reco track first (both barrel and endcap tracks allowed)
      float isoET1=TB.cluster.ET /TB.cl4x4.ET;
      hA[51]->Fill(isoET1);
      hA[52]->Fill(TB.cluster.ET);
      hA[50]->Fill("trB",1.); 
      if(TB.cluster.ET<par_clusterEtZ) continue;
      hA[50]->Fill("etB",1.); 

      float fracET1=TB.cluster.ET /TB.nearTotET;
      hA[53]->Fill(fracET1);
      if(fracET1<par_nearTotEtFracZ) continue; 
      hA[50]->Fill("conB",1.); 

      // 1) try to find candidate track in the endcap
      for(uint it=0;it<V.eleTrack.size();it++) {
	WeveEleTrack &TE=V.eleTrack[it];
	if(TE.pointTower.id>=0) continue; //skip barrel towers
	if(TE.isMatch2Cl==false) continue;
	assert(TE.cluster.nTower>0); // internal logical error
	assert(TE.nearTotET>0); // internal logical error

	float isoET2=TE.cluster.ET/TE.cl4x4.ET;
	hA[71]->Fill(isoET2);
	hA[72]->Fill(TE.cluster.ET);
	hA[70]->Fill("trE",1.);
	if(TE.cluster.ET<par_clusterEtZ) continue;
	hA[70]->Fill("etE",1.);

	float fracET2=TE.cluster.ET/TE.nearTotET;
	hA[73]->Fill(fracET2);
	if(fracET2<par_nearTotEtFracZ) continue; 
	hA[70]->Fill("conE",1.);

	float e1=TB.cluster.energy;
	float e2=TE.cluster.energy;
	TVector3 p1=TB.primP; p1.SetMag(e1);//cluster.position;
	TVector3 p2=TE.primP; p2.SetMag(e2);//cluster.position;

	float del_phi=p1.DeltaPhi(p2);
	//printf("del Phi=%f\n",del_phi);
	float xx=del_phi;
	if(xx<-PI+1) xx+=2*PI;
	hA[74]->Fill(xx);
	if(fabs(del_phi)<par_delPhi12) continue;
	hA[70]->Fill("phi12",1.);

	TVector3 psum=p1+p2;
	float mass2=(e1+e2)*(e1+e2)-(psum.Dot(psum));
	if(mass2<1.) continue; // 9GeV^2) should be param, I'm tired today
	hA[70]->Fill("m2",1.);
	hA[77]->Fill(p1.Eta(),p2.Eta());
	hA[78]->Fill(psum.Eta());
	hA[79]->Fill(psum.Pt());

	float mass=sqrt(mass2);
	int Q1Q2=TB.prMuTrack->charge()*TE.prMuTrack->charge();
	if (Q1Q2==1) { //..  same sign , can't be Z-> e+ e-
	  hA[76]->Fill(mass);
	  hA[80]->Fill(TE.cluster.ET,TE.prMuTrack->charge()/TE.prMuTrack->pt());	  continue;
	}

	//..... now only opposite sign
	hA[70]->Fill("QQ",1.);
	hA[75]->Fill(mass);
	hA[81]->Fill(TE.cluster.ET,TE.prMuTrack->charge()/TE.prMuTrack->pt());
      }

      // 2) use highest ET endcap cluster with no track requirement
      float maxET=0; 
      WeveCluster maxCluster;
      for(int iEta=0; iEta<12; iEta++){ //loop over eta bins
	for(int iPhi=0; iPhi<60; iPhi++){ //loop over phi bins
	  
	  WeveCluster eclust=wMK->maxEtow2x2(iEta,iPhi,V.z);
	  if(eclust.ET < par_clusterEtZ) continue;
	  if(maxET > eclust.ET) continue;
	  else {
	    maxET=eclust.ET;
	    maxCluster=eclust;
	  }
	}
      }
      if(maxCluster.ET <= 1.0) continue;//remove low E clusters

      //apply cuts to max ETOW cluster and isolation sums
      WeveCluster cl4x4=wMK->sumEtowPatch(maxCluster.iEta-1,maxCluster.iPhi-1,4,4,V.z);
      hA[54]->Fill(maxCluster.ET/cl4x4.ET);
      if(maxCluster.ET/cl4x4.ET<wMK->parE_clustFrac24) continue;
      hA[55]->Fill(maxCluster.ET);
      hA[50]->Fill("trE",1.);
      if(maxCluster.ET < par_clusterEtZ) continue;
      hA[50]->Fill("etE",1.);
      
      //assume poor tracking effic so only towers in nearCone
      float nearBtow=wMK->sumBtowCone(V.z,maxCluster.position,2);
      float nearEtow=wMK->sumEtowCone(V.z,maxCluster.position,2);
      float nearSum=nearBtow; nearSum+=nearEtow;
      hA[56]->Fill(maxCluster.ET/nearSum);
      if(maxCluster.ET/nearSum<wMK->parE_nearTotEtFrac) continue;
      hA[50]->Fill("conE",1.);
      
      //add plots of good candidates
      float e1=TB.cluster.energy;
      float e2=maxCluster.energy;
      TVector3 p1=TB.primP; p1.SetMag(e1);
      TVector3 p2=maxCluster.position; p2.SetMag(e2);
      float del_phi=p1.DeltaPhi(p2);
      float xx=del_phi;
      if(xx<-PI+1) xx+=2*PI;
      hA[57]->Fill(xx);
      if(fabs(del_phi)<par_delPhi12) continue;
      hA[50]->Fill("phi12",1.);
      TVector3 psum=p1+p2;
      float mass2=(e1+e2)*(e1+e2)-(psum.Dot(psum));
      if(mass2<1.) continue; 
      hA[50]->Fill("m2",1.);
      float mass=sqrt(mass2);
      hA[58]->Fill(mass);
      hA[59]->Fill(p1.Eta(),p2.Eta());
      hA[60]->Fill(psum.Eta());
      hA[61]->Fill(psum.Pt());

    } //track loop
  } //vertex loop
}


//_____________________________________________________________________________
//
void
St2011ZMaker::find_Z_boson(){
  Wevent2011 *wEve=wMK->wEve;
  // printf("========= find_Z_boson() \n");
  
  hA[31]->Fill(wEve->vertex.size());
  hA[0]->Fill("inp",1.);

  // search for  Zs ............
  for(uint iv=0;iv<wEve->vertex.size();iv++) {
    hA[0]->Fill("vert",1.);
    WeveVertex &V=wEve->vertex[iv];
    hA[32]->Fill(V.eleTrack.size());
    if(V.eleTrack.size()<2) continue;
    hA[0]->Fill("TT",1.); // at least 2 isolated tracks exist

    //only one Z can come from a vertex, and it should be the highest-energy object
    //hence, the two highest-et clusters should correspond to the z.  Pick those 
    //eventually, but for now, just try all of them.
    for(uint it=0;it<V.eleTrack.size()-1;it++) { //.....select first track:
      WeveEleTrack &T1=V.eleTrack[it];
      if(T1.pointTower.id<=0) continue; //skip endcap towers
      if(T1.isMatch2Cl==false) continue;
      assert(T1.cluster.nTower>0); // internal logical error
      assert(T1.nearTotET>0); // internal logical error

      float isoET1=T1.cluster.ET /T1.cl4x4.ET;
      hA[29]->Fill(isoET1);

      hA[23]->Fill(T1.cluster.ET);
      hA[0]->Fill("tr1",1.);
      if(T1.cluster.ET<par_clusterEtZ) continue;
      hA[0]->Fill("et1",1.);

      float fracET1=T1.cluster.ET /T1.nearTotET;
      hA[24]->Fill(fracET1);
      if(fracET1< par_nearTotEtFracZ) continue; 
      hA[0]->Fill("con1",1.);

      for (uint it2=it+1;it2<V.eleTrack.size();it2++) {	//.....select second track:
	WeveEleTrack &T2=V.eleTrack[it2];
	if(T2.pointTower.id<=0) continue; //skip endcap towers
	if(T2.isMatch2Cl==false) continue;
	assert(T2.cluster.nTower>0); // internal logical error
	assert(T2.nearTotET>0); // internal logical error

	float isoET2=T2.cluster.ET /T2.cl4x4.ET;
	hA[30]->Fill(isoET2);

	hA[25]->Fill(T2.cluster.ET);
	hA[0]->Fill("tr2",1.);
	if(T2.cluster.ET<par_clusterEtZ) continue;
	hA[0]->Fill("et2",1.);

	float fracET2=T2.cluster.ET /T2.nearTotET;
	hA[26]->Fill(fracET2);
	if(fracET2< par_nearTotEtFracZ) continue; 
	hA[0]->Fill("con2",1.);

	float e1=T1.cluster.energy;
	float e2=T2.cluster.energy;
	TVector3 p1=T1.primP; p1.SetMag(e1);//cluster.position;
	TVector3 p2=T2.primP; p2.SetMag(e2);//cluster.position;

	float del_phi=p1.DeltaPhi(p2);
	//printf("del Phi=%f\n",del_phi);
	float xx=del_phi;
	if(xx<-PI+1) xx+=2*PI;
	hA[27]->Fill(xx);
	if(fabs(del_phi)<par_delPhi12) continue;
	hA[0]->Fill("phi12",1.);

	TVector3 psum=p1+p2;
	float mass2=(e1+e2)*(e1+e2)-(psum.Dot(psum));
	float yZ=0.5*log((e1+e2+psum.Z())/(e1+e2-psum.Z()));
	if(mass2<1.) continue; // 9GeV^2) should be param, I'm tired today
	hA[0]->Fill("m2",1.);
	
	float mass=sqrt(mass2);
	int Q1Q2=T1.prMuTrack->charge()*T2.prMuTrack->charge();
	if (Q1Q2==1) { //..  same sign , can't be Z-> e+ e-
	  hA[14]->Fill(mass);
	  continue;
	}

	//..... now only opposite sign
	hA[0]->Fill("QQ",1.);
	hA[15]->Fill(mass);
	hA[33]->Fill(T1.cluster.ET,T1.prMuTrack->charge()/T1.prMuTrack->pt()); 
	hA[33]->Fill(T2.cluster.ET,T2.prMuTrack->charge()/T2.prMuTrack->pt()); 
	hA[34]->Fill(T1.pointTower.iEta ,T1.cluster.energy);
	hA[34]->Fill(T2.pointTower.iEta ,T2.cluster.energy);
	hA[35]->Fill(p1.Eta(),p2.Eta());
	
	if(T1.prMuTrack->charge()>0) hA[42]->Fill(p1.Phi(),p2.Phi());
	else hA[42]->Fill(p2.Phi(),p1.Phi());
	hA[43]->Fill(T1.cluster.ET,T1.prMuTrack->charge()*T1.cluster.ET/T1.prMuTrack->pt()); 
	hA[43]->Fill(T2.cluster.ET,T2.prMuTrack->charge()*T2.cluster.ET/T2.prMuTrack->pt()); 

#if 0
	printf("RCC:  Found Z w/ invmass=%f\n",mass);
        printJan(&T1);
        printJan(&T2);


	if (!wMK->isMC || (wMK->isMC&& wEve->id<500) )
	  { printf("\n ZZZZZZZZZZZZZZZZZZZ\n");
	    if(mass<par_minMassZ) 
	    wMK->wDisaply->exportEvent("Zlow",V,T1);
	    else
	      wMK->wDisaply->exportEvent("Zgood",V,T1);
	    printf("RCC:  Found Z w/ invmass=%f\n",mass);
	    wEve->print();
	  }
	
#endif

	if (mass<par_minMassZ) continue; //enforce a lower bound
	hA[0]->Fill("Zlow",1.);

	if (mass>par_maxMassZ) continue; //enforce an upper bound
	hA[0]->Fill("Zhigh",1.);

	hA[36]->Fill(yZ);//psum.Eta());
	hA[37]->Fill(psum.Pt());
	
	int bxStar7=wMK->wEve->bxStar7;
	int bxStar48=wMK->wEve->bxStar48; 
	if(bxStar48!=bxStar7) {
	  hA[0]->Fill("badBx48",1.);
	  return; // both counters must be in sync
	}
	
	int spin4=wMK->wEve->spin4;  
	hA[38]->Fill(spin4); 
	if(yZ<0) hA[39]->Fill(spin4);
	else if(yZ>0) hA[40]->Fill(spin4);

	// L0 x1 and x2 computation
	float mZ = 91.188; 
	float roots = 510.;
	float x1 = mZ/roots * TMath::Exp(yZ);
	float x2 = mZ/roots * TMath::Exp(-1.*yZ);
	hA[44]->Fill(x1,x2);
	hA[45]->Fill(x1*mass/mZ,x2*mass/mZ);

	// free quark search
	if(T1.prMuTrack->charge()>0) 
	  hA[46]->Fill( T1.prMuTrack->charge()*T1.cluster.ET/T1.prMuTrack->pt(),
			T2.prMuTrack->charge()*T2.cluster.ET/T2.prMuTrack->pt());
	else
	  hA[46]->Fill( T2.prMuTrack->charge()*T2.cluster.ET/T2.prMuTrack->pt(),
			T1.prMuTrack->charge()*T1.cluster.ET/T1.prMuTrack->pt());
	
	// **** I stoped changes here, Jan 
	
	float fmax1=T1.cluster.ET/T1.cl4x4.ET;
	float fmax2=T2.cluster.ET/T2.cl4x4.ET;

	hA[21]->Fill(fmax1,fmax2);
	hA[22]->Fill(T1.cluster.ET,T2.cluster.ET);

	hA[1]->Fill(mass);
	hA[2]->Fill(T1.prMuTrack->charge(),T2.prMuTrack->charge());
	hA[3]->Fill(T1.prMuTrack->charge()*T2.prMuTrack->charge());
	hA[4]->Fill(p1.Phi(),p2.Phi());
	hA[5]->Fill(del_phi);
	hA[6]->Fill(mass,T1.prMuTrack->charge()/T1.primP.Perp()*T2.prMuTrack->charge()/T1.primP.Perp());
	hA[7]->Fill(mass,T1.prMuTrack->charge()*T2.prMuTrack->charge());
	hA[8]->Fill(T1.cluster.ET);
	if (T1.prMuTrack->charge()>0)
	  {
	    hA[9]->Fill(p1.Eta(),p1.Phi());
	    hA[10]->Fill(p2.Eta(),p2.Phi());
	  }
	else
	  {
	    hA[10]->Fill(p1.Eta(),p1.Phi());
	    hA[9]->Fill(p2.Eta(),p2.Phi());
	  }

	hA[11]->Fill(fmax1,fmax2);
	hA[12]->Fill(T1.cluster.ET,T2.cluster.ET);
	hA[13]->Fill(mass,del_phi);

      }


    
    }// loop over first track
  }// loop over vertices
 
}


// $Log: St2011ZMaker.cxx,v $
// Revision 1.13  2016/01/08 02:08:49  jlzhang
// added couples histograms and fixed a small bug
//
// Revision 1.12  2012/10/05 17:53:53  balewski
// added correlation plots for reco Q in Z, W algos
//
// Revision 1.11  2012/10/05 16:44:31  stevens4
// final z plots: update x1-x2 correlation in z maker
//
// Revision 1.10  2012/10/01 19:48:20  stevens4
// add plots for Z result and move esmd cross point calculation outside plane loop
//
// Revision 1.9  2012/09/26 14:20:59  stevens4
// use PtBal cos(phi) for WB and WE algos and use Q*ET/PT for barrel charge sign
//
// Revision 1.8  2012/09/24 19:28:15  balewski
// moved eta & pT hitso to be filled after invM cut, now they are 'golden Z'
//
// Revision 1.7  2012/09/18 19:34:22  balewski
// fill eta-dependent spin sort
//
// Revision 1.6  2012/09/14 21:02:29  balewski
// *lumi-maker re-written to accumulate alternative rel lumi monitors,
// * added spin sorting to Zs
//
// Revision 1.5  2012/08/21 17:40:09  stevens4
// Revert to previous version
//
// Revision 1.3  2012/06/29 21:20:08  stevens4
// *** empty log message ***
//
// Revision 1.2  2012/06/26 20:30:23  stevens4
// Updates ZMaker for mixing barrel and endcap arms
//
// Revision 1.1  2011/02/10 20:33:24  balewski
// start
//
