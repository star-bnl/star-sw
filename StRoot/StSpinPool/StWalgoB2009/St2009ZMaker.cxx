// $Id: St2009ZMaker.cxx,v 1.7 2010/01/06 05:21:57 balewski Exp $
//
//*-- Author : Ross Corliss, MIT
//  changes Jan Balewski, MIT
// 

#include "St2009WMaker.h"
#include "WeventDisplay.h"
#include "St2009ZMaker.h"

ClassImp(St2009ZMaker)

//_____________________________________________________________________________
//
St2009ZMaker::St2009ZMaker(const char *name):StMaker(name){
  wMK=0;muMK=0;HList=0;

}


//_____________________________________________________________________________
//
Int_t St2009ZMaker::Init(){
  assert(wMK);
  assert(HList);
  initHistos();
  return StMaker::Init();
}


//_____________________________________________________________________________
//
Int_t St2009ZMaker::InitRun  (int runumber){
  LOG_INFO<<Form("::InitRun(%d) done, Z-algo params: nearTotEtFrac=%.2f,  clusterEt=%.1f GeV, delPhi12>%.2f rad, Zmass in[%.1f,%.1f]\n",
		 runumber, par_nearTotEtFracZ,par_clusterEtZ,par_delPhi12,par_minMassZ,par_maxMassZ)<<endm;
  return 0;
}

//_____________________________________________________________________________
//
Int_t St2009ZMaker::FinishRun  (int runnumber){
return 0;
}

//_____________________________________________________________________________
//
Int_t 
St2009ZMaker::Make(){

  //fill various histograms and arrays with event data
  find_Z_boson();

  return kStOK;
}

//============================
void
St2009ZMaker::printJan(WeveEleTrack *T) {
  int ibp=kBTow;
  WevePointTower poiTw=T->pointTower;
  WeveCluster cl=T->cluster;
  int id= poiTw.id;
  float adc= wMK->wEve.bemc.adcTile[ibp][id-1];
  float frac= adc/4096*60 /cl.ET;
  printf("Ztower Q=%d pointTw: id=%d ADC=%.0f  2x2ET=%.1f frac=%.2f\n",T->prMuTrack->charge(),id,adc,cl.ET,frac);
}



//_____________________________________________________________________________
//
void
St2009ZMaker::find_Z_boson(){
  const float PI=TMath::Pi();
  Wevent2009 &wEve=wMK->wEve;
  // printf("========= find_Z_boson() \n");
  
  hA[31]->Fill(wEve.vertex.size());
  hA[0]->Fill("inp",1.);

  // search for  Zs ............
  for(uint iv=0;iv<wEve.vertex.size();iv++) {
    hA[0]->Fill("vert",1.);
    WeveVertex &V=wEve.vertex[iv];
    hA[32]->Fill(V.eleTrack.size());
    if(V.eleTrack.size()<2) continue;
    hA[0]->Fill("TT",1.); // at least 2 isolated tracks exist

    //only one Z can come from a vertex, and it should be the highest-energy object
    //hence, the two highest-et clusters should correspond to the z.  Pick those 
    //eventually, but for now, just try all of them.
    for(uint it=0;it<V.eleTrack.size()-1;it++) { //.....select first track:
      WeveEleTrack &T1=V.eleTrack[it];
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
	if(mass2<1.) continue; // 9GeV^2) should be param, I'm tired today
	hA[0]->Fill("m2",1.);

	float mass=sqrt(mass2);
	int Q1Q2=T1.prMuTrack->charge()*T2.prMuTrack->charge();
	if (Q1Q2==1) { //..  same sign 
	  hA[14]->Fill(mass);
	  continue;
	}
	printf("RCC:  Found Z! invmass=%f\n",mass);
        printJan(&T1);
        printJan(&T2);

	//..... now only opposite sign
	hA[0]->Fill("QQ",1.);
	hA[15]->Fill(mass);
	hA[33]->Fill(T1.cluster.ET,T1.prMuTrack->charge()/T1.prMuTrack->pt()); 
	hA[33]->Fill(T2.cluster.ET,T2.prMuTrack->charge()/T2.prMuTrack->pt()); 

	if (mass<par_minMassZ) continue; //enforce a lower bound
	hA[0]->Fill("Zlow",1.);

	if (mass>par_maxMassZ) continue; //enforce an upper bound
	hA[0]->Fill("Zhigh",1.);

	// **** I stoped changes here, Jan 

	float fmax1=T1.cluster.ET/T1.cl4x4.ET;
	float fmax2=T2.cluster.ET/T2.cl4x4.ET;

	hA[21]->Fill(fmax1,fmax2);
	hA[22]->Fill(T1.cluster.ET,T2.cluster.ET);


	if (!wMK->isMC || (wMK->isMC&& wEve.id<500) )
	  { printf("\n ZZZZZZZZZZZZZZZZZZZ\n");
	    wMK->wDisaply->exportEvent("Z",V,T1);
	    printf("RCC:  Found Z! invmass=%f\n",mass);
	    wEve.print();
	  }

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


// $Log: St2009ZMaker.cxx,v $
// Revision 1.7  2010/01/06 05:21:57  balewski
// cleanup
//
// Revision 1.6  2010/01/06 04:22:15  balewski
// added Q/PT plot for Zs, more cleanup
//
// Revision 1.5  2010/01/05 03:22:55  balewski
// change logic for filling btow status tables, added printout to Z-code
//
// Revision 1.4  2010/01/04 05:12:00  balewski
// added 4x4 cut to Z-algo, cleanup
//
// Revision 1.3  2010/01/03 04:38:24  balewski
// reorganized Z-algo
//
// Revision 1.2  2009/12/11 20:37:09  rcorliss
// Updated Z Maker to use track direction rather than tower direction when computing invariant mass.  Added new histograms.
//
// Revision 1.1  2009/12/07 20:37:56  rcorliss
// Start
//
