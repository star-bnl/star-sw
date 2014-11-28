// $Id: St2009pubJSMaker.cxx,v 1.2 2010/03/14 22:50:31 balewski Exp $
//
//*-- Author :  Justin Stevens, IUCF
// 

#include "St2009WMaker.h"
#include "St2009pubJSMaker.h"

ClassImp(St2009pubJSMaker)

//_______________________________________________________
//_______________________________________________________
St2009pubJSMaker::St2009pubJSMaker(const char *name):StMaker(name){
  wMK=0;HList=0;
  par_highEtow=5; // GeV
  par_awayNTrCut=2;
  
}


//_______________________________________________________
//_______________________________________________________
St2009pubJSMaker::~St2009pubJSMaker(){
  //
}


//_______________________________________________________
//_______________________________________________________
Int_t St2009pubJSMaker::Init(){
  assert(wMK);
  par_countTrPt=wMK->par_countTrPt;
  par_countTowEt=wMK->par_countTowEt;
  par_awayTotET=30; // old, clean it up, Jan
  assert(HList);
  initHistos();
  return StMaker::Init();
}


//_______________________________________________________
//_______________________________________________________
Int_t 
St2009pubJSMaker::Make(){
  //printf("-----------in %s\n", GetName());
    
  doWanalysis();
  
  return kStOK;
}

//_______________________________________________________
//_______________________________________________________
void 
St2009pubJSMaker::doWanalysis(){
  //has access to whole W-algo-maker data via pointer 'wMK'
 
  // run through W cuts to fill other histos............
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      if(it==0) etowQA(0,V.z); //if there is a reco track do etowQA
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error
      
      if(T.cluster.ET /T.nearTotET< wMK->par_nearTotEtFrac) continue; // too large nearET
      if(T.awayTotET> par_awayTotET) continue; // too large awayET, Jan
      //Transverse mass reco

      //hadronic recoil 
      TVector3 hadronicPt(T.hadronicRecoil.X(),T.hadronicRecoil.Y(),0); //transvers momentum vector
      hA[25]->Fill(hadronicPt.Perp()); 
      hA[26]->Fill(T.hadronicRecoil.Eta());
      hA[27]->Fill(T.cluster.ET);      

      TVector3 electronPt(T.cluster.position.X(),T.cluster.position.Y(),0); //transvers momentum vector
      electronPt.SetMag(T.cluster.ET);

      //neutrino reco 
      TVector3 neutrinoPt=-1*(hadronicPt+electronPt);
      hA[28]->Fill(neutrinoPt.Perp());  

      float recoDeltaPhi=neutrinoPt.DeltaPhi(electronPt);
      float Mt=sqrt(2*T.cluster.ET*neutrinoPt.Perp()*(1-cos(recoDeltaPhi)));  
      hA[29]->Fill(Mt); //real data
      hA[30]->Fill(Mt,T.cluster.ET);

      if(T.cluster.ET < 30) continue; //enrich W sample
      //Kinematics plots
      float eleEta=T.primP.Eta();
      if(eleEta < -0.8) 
	hA[31]->Fill(T.cluster.energy);
      if(eleEta > 0.8)
	hA[32]->Fill(T.cluster.energy);
      if(eleEta > -0.1 && eleEta < 0.1)
	hA[33]->Fill(T.cluster.energy);

      //Can we reco W pL?
      float eleTheta=T.primP.Theta();
      float ratioE=T.cluster.energy/40;  
      float pLRecoPlus=80*ratioE*((cos(eleTheta))+sqrt(cos(eleTheta)*cos(eleTheta)+sin(eleTheta)*sin(eleTheta)*(1-ratioE*ratioE)))/(ratioE*ratioE*sin(eleTheta)*sin(eleTheta));//+ solution
      float pLRecoMinus=80*ratioE*((cos(eleTheta))+sqrt(cos(eleTheta)*cos(eleTheta)+sin(eleTheta)*sin(eleTheta)*(1-ratioE*ratioE)))/(ratioE*ratioE*sin(eleTheta)*sin(eleTheta));//+ solution
      hA[34]->Fill(pLRecoPlus);
      hA[35]->Fill(pLRecoMinus);


      //Other stuff in pubJS
      hA[11]->Fill(T.nearNTow); 
      hA[12]->Fill(T.nearNTr);

      hA[24]->Fill(T.cluster.ET/T.nearTotET,T.cluster.position.PseudoRapidity());
      
      etowQA(1,-999); //before near cut do etowQA
      if(T.cluster.ET /T.nearTotET< wMK->par_nearTotEtFrac) continue; // too large nearET
      
      hA[7]->Fill(T.awayNTow); 
      hA[8]->Fill(T.awayNTr);

      etowQA(2,-999); //before away cut do etowQA
      if(T.awayTotET> par_awayTotET) { // too large awayET, Jan
	if(T.awayNTr > par_awayNTrCut)  
	  hA[9]->Fill(T.cluster.ET);   
	else                           
 	  hA[10]->Fill(T.cluster.ET);  
	continue; 
      }
      
      if(T.cluster.ET < 25){ //should be all background
	hA[13]->Fill(V.z);
	hA[14]->Fill(T.awayTotET);
	hA[15]->Fill(T.nearNTow); 
	hA[16]->Fill(T.nearNTr);
	
	hA[18]->Fill(V.id);
	hA[19]->Fill(V.funnyRank);
	hA[20]->Fill(wMK->wEve.bx48);
	hA[21]->Fill(wMK->wEve.bx7);
	hA[22]->Fill(T.cluster.position.PseudoRapidity(),T.cluster.position.Phi());
	hA[23]->Fill(T.cluster.nTower);
      }

    }
  }
}


//________________________________________________
//________________________________________________
void
St2009pubJSMaker::etowQA(int whichCut,float zVert){//whichCut gives where in algo you are calling etowQA
  
  //loop over all endcap towers
  for(int iphi=0;iphi<60;iphi++){
    for(int ieta=0;ieta<12;ieta++){
      TVector3 towerPos=wMK->positionEtow[iphi][ieta];
      float ene=wMK->wEve.etow.ene[iphi][ieta];
      int etabin=abs(ieta-11);//reverse ie. now etabin[0,11] -> phys eta[1,2]
      //if(iphi==0) cout<<"iphi = "<<iphi<<" phi = "<<towerPos.Phi()<<" ieta = "<<ieta<<" eta = "<<towerPos.Eta()<<" etabin = "<<etabin<<endl;

      //get ET for test
      TVector3 primP=towerPos-TVector3(0,0,zVert);
      primP.SetMag(ene); // it is 3D momentum in the event ref frame
      float ET=primP.Perp();

      hB[whichCut]->Fill(etabin,towerPos.Phi(),ene); 
      if(ET > par_highEtow) //high tower
	hA[whichCut]->Fill(etabin,towerPos.Phi());
    
      // Look at vertex Z dependence of energy for all reco vertices that have a track with pT > 10
      if(whichCut==0 && etabin==11){//only once for all events
	if(zVert>-100  && zVert<=-50)
	  hA[3]->Fill(ene);
	else if(zVert>-50  && zVert<=0)
	  hA[4]->Fill(ene);
	else if(zVert>0  && zVert<=50)
	  hA[5]->Fill(ene);
	else if(zVert>50  && zVert<=100)
	  hA[6]->Fill(ene);
      }


    }
  }
  return;
}



// $Log: St2009pubJSMaker.cxx,v $
// Revision 1.2  2010/03/14 22:50:31  balewski
// *** empty log message ***
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
