// $Id: St2009ZMaker.cxx,v 1.1 2009/12/07 20:37:56 rcorliss Exp $
//
//*-- Author : Jan Balewski, MIT
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
  //  printf("in %s\n", GetName());
  //hA[0]->Fill("",1.);

  //fill various histograms and arrays with event data
  find_Z_boson();

  return kStOK;
}

//_____________________________________________________________________________
//
void
St2009ZMaker::find_Z_boson(){
  Wevent2009 &wEve=wMK->wEve;
  // printf("========= find_Z_boson() \n");
  
  hA[9]->Fill(wEve.vertex.size());

  // search for  Zs ............
  for(uint iv=0;iv<wEve.vertex.size();iv++) {
    WeveVertex &V=wEve.vertex[iv];
    hA[10]->Fill(V.eleTrack.size());
    //only one Z can come from a vertex, and it should be the highest-energy object
    //hence, the two highest-et clusters should correspond to the z.  Pick those 
    //eventually, but for now, just try all of them.
    for(uint it=0;it<V.eleTrack.size();it++) {
      //select first track:
      WeveEleTrack &T1=V.eleTrack[it];
      if(T1.isMatch2Cl==false) continue;
      assert(T1.cluster.nTower>0); // internal logical error
      assert(T1.nearTotET>0); // internal logical error
      hA[7]->Fill(T1.cluster.ET);
      //if(T1.cluster.ET /T1.nearTotET< par_nearTotEtFracZ) continue; // too large nearET


      for (uint it2=it+1;it2<V.eleTrack.size();it2++) {
	//select second track:
	WeveEleTrack &T2=V.eleTrack[it2];
	if(T2.isMatch2Cl==false) continue;
	assert(T2.cluster.nTower>0); // internal logical error
	assert(T2.nearTotET>0); // internal logical error

	float e1=T1.cluster.energy;
	float e2=T2.cluster.energy;
	TVector3 p1=T1.cluster.position; p1.SetMag(e1);
	TVector3 p2=T2.cluster.position; p2.SetMag(e2);
	TVector3 psum=p1+p2;
	float mass_sqrd=(e1+e2)*(e1+e2)-(psum.Dot(psum));
	float mass=sqrt(mass_sqrd);
	float fmax1=T1.cluster.ET/T1.cl4x4.ET;
	float fmax2=T2.cluster.ET/T2.cl4x4.ET;
	//float phi_diff=(p1.Phi()-p2.Phi()+2*TMath::Pi())%(2*Tmath::Pi());
	float rel_phi=sqrt((p1.Phi()-p2.Phi())*(p1.Phi()-p2.Phi()));

	hA[40]->Fill(mass>0?mass:0);//the greater of mass and 0, just in case.

	//this first cut could be moved out of the internal for loop, but I need it here
	//so that it can be used in the stacked plot.
	if(T1.cluster.ET /T1.nearTotET< par_nearTotEtFracZ) continue; // too large nearET
	hA[41]->Fill(mass_sqrd>0?mass:0);

	if(T2.cluster.ET /T2.nearTotET< par_nearTotEtFracZ) continue; // too large nearET
	hA[42]->Fill(mass_sqrd>0?mass:0);

	if(T1.cluster.ET<par_clusterEtZ || T2.cluster.ET <par_clusterEtZ) continue;
	hA[43]->Fill(mass_sqrd>0?mass:0);

	if (mass_sqrd<0) continue; //floating point fluctuation of colinear.
	hA[44]->Fill(mass);

	if (mass<par_minMassZ) continue; //enforce a lower bound
	hA[45]->Fill(mass);

	if (mass>par_maxMassZ) continue; //enforce an upper bound
	hA[46]->Fill(mass);

	if (!wMK->isMC)
	  {
	    wMK->wDisaply->exportEvent(V,T1);
	    printf("RCC:  Found Z! invmass=%f\n",mass);
	    wEve.print();
	  }

	hA[1]->Fill(mass);
	hA[2]->Fill(T1.prMuTrack->charge(),T2.prMuTrack->charge());
	hA[3]->Fill(T1.prMuTrack->charge()*T2.prMuTrack->charge());
	hA[4]->Fill(rel_phi);
	hA[11]->Fill(mass,T1.prMuTrack->charge()*T2.prMuTrack->charge());

	hA[5]->Fill(fmax1,fmax2);
	hA[6]->Fill(T1.cluster.ET,T2.cluster.ET);

	hA[8]->Fill(mass,rel_phi);
	hA[12]->Fill(p1.Phi(),p2.Phi());
	hA[13]->Fill(T1.cluster.ET);
	if (T1.prMuTrack->charge()>0)
	  {
	    hA[14]->Fill(p1.Eta(),p1.Phi());
	    hA[15]->Fill(p2.Eta(),p2.Phi());
	  }
	else
	  {
	    hA[15]->Fill(p1.Eta(),p1.Phi());
	    hA[14]->Fill(p2.Eta(),p2.Phi());
	  }
      }


    
    }// loop over first track
  }// loop over vertices
 
}


// $Log: St2009ZMaker.cxx,v $
// Revision 1.1  2009/12/07 20:37:56  rcorliss
// Start
//
