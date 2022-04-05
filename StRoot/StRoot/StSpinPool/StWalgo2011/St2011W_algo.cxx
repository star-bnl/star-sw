// $Id: St2011W_algo.cxx,v 1.22 2016/01/08 02:08:49 jlzhang Exp $
//
//*-- Author : Jan Balewski, MIT
//*-- Author for Endcap: Justin Stevens, IUCF

#include "TF1.h"

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "WeventDisplay.h"

//new jet tree format
#include "StSpinPool/StJetEvent/StJetEvent.h"
#include "StSpinPool/StJetEvent/StJetVertex.h"
#include "StSpinPool/StJetEvent/StJetCandidate.h"
#include "StSpinPool/StJetEvent/StJetTower.h"

#include "St2011WMaker.h"

//________________________________________________
//________________________________________________
void
St2011WMaker::find_W_boson(){

  if(!wEve->l2bitET) return;

  //printf("========= find_W_boson() \n");
  int nNoNear=0,nNoAway=0,nEta1=0,nGoldW=0,nGoldWp=0,nGoldWn=0;
  //remove events tagged as Zs
  if(wEve->zTag) return;

  // search for  Ws ............
  for(uint iv=0;iv<wEve->vertex.size();iv++) {
	WeveVertex &V=wEve->vertex[iv];
	for(uint it=0;it<V.eleTrack.size();it++) {
	  WeveEleTrack &T=V.eleTrack[it];

	  getJetEvent();
	  hA[114]->Fill(T.cluster.position.PseudoRapidity(),T.sPtBalance2);
	  hA[115]->Fill(T.cluster.position.Phi(),T.sPtBalance2);

	  if(T.pointTower.id<=0) continue; //skip endcap towers
	  if(T.isMatch2Cl==false) continue;
	  assert(T.cluster.nTower>0); // internal logical error
	  assert(T.nearTotET>0); // internal logical error

	  //make cut on lepton eta 
	  if(T.primP.Eta() < par_leptonEtaLow || T.primP.Eta() > par_leptonEtaHigh) continue;      
	  hA[20]->Fill("eta1",1.);
	  nEta1++;

	  //define charge separation
	  float q2pt_g = T.glMuTrack->charge()/T.glMuTrack->pt();
	  float q2pt_p = T.prMuTrack->charge()/T.prMuTrack->pt();
	  float hypCorr_g = q2pt_g*(T.cluster.ET);
	  float hypCorr_p = q2pt_p*(T.cluster.ET);

	  //remove ambiguous charges from BG treatment histos
	  if( fabs(hypCorr_p) > par_QET2PTlow && fabs(hypCorr_p) < par_QET2PThigh) {
		//signal plots w/o EEMC in awayside veto
		if(T.cluster.ET/T.nearTotET_noEEMC>par_nearTotEtFrac){
		  if(T.sPtBalance_noEEMC2>par_ptBalance ) {//only signed ptBalance cut 
			hA[140]->Fill(T.cluster.ET);
			hA[240]->Fill(T.prMuTrack->eta(),T.cluster.ET);
			if (T.prMuTrack->charge() < 0) {
			  hA[184+3]->Fill(T.cluster.ET);
			} else if (T.prMuTrack->charge() > 0) {
			  hA[184+4]->Fill(T.cluster.ET);
			}
		  }
		}
	  }

	  //fill plot for background
	  if(T.cluster.ET > par_highET) {
		if(T.prMuTrack->charge()>0) hA[251]->Fill(T.cluster.ET/T.nearTotET,T.sPtBalance2);
		else if(T.prMuTrack->charge()<0) hA[252]->Fill(T.cluster.ET/T.nearTotET,T.sPtBalance2);
		hA[135]->Fill(T.awayTotET,T.sPtBalance2);
	  }

	  // track matched to cluster plots
	  StThreeVectorF ri=T.glMuTrack->firstPoint();
	  StThreeVectorF ro=T.glMuTrack->lastPoint();
	  int sec = WtpcFilter::getTpcSec(ro.phi(),ro.pseudoRapidity());
	  if((sec < 5 || sec > 7) && sec!=21) { //skip sectors with dead padrows for this
		hA[63]->Fill(T.prMuTrack->nHitsFit());
		hA[64]->Fill(1.*T.prMuTrack->nHitsFit()/T.prMuTrack->nHitsPoss());
		hA[65]->Fill(ri.perp());
	  }

	  if(T.cluster.ET /T.nearTotET< par_nearTotEtFrac) continue; // too large nearET

	  hA[20]->Fill("noNear",1.);
	  nNoNear++;
	  hA[112]->Fill( T.cluster.ET); // for Joe
	  hA[50]->Fill(T.awayTpcPT);
	  hA[51]->Fill(T.awayBtowET);
	  hA[54]->Fill(T.awayTotET); 
	  hA[52]->Fill(T.cluster.ET,T.awayTotET);
	  hA[53]->Fill(T.cluster.ET,T.awayEmcET);
	  hA[55]->Fill(T.awayEtowET);
	  hA[60]->Fill(T.cluster.ET,T.awayTpcPT);

	  hA[132]->Fill(T.cluster.ET,T.ptBalance.Perp());
	  hA[133]->Fill(T.awayTotET,T.ptBalance.Perp());
	  hA[134]->Fill(T.cluster.ET,T.sPtBalance);
	  hA[135]->Fill(T.awayTotET,T.sPtBalance);
	  hA[209]->Fill(T.cluster.position.PseudoRapidity(),T.cluster.ET);
	  if(T.cluster.ET > par_highET) hA[253]->Fill(T.awayTotET,T.sPtBalance);



	  //if(T.cluster.ET> par_highET && T.awayTotET>par_awayET) continue;

	  // add 2 histograms hA[138],hA[139], the charged separation sPtBalance vs Et 2D plots for background estimation; 
	  // Jinlong 12/09/2014
	  if(T.prMuTrack->charge()>0) {
		hA[130]->Fill(T.cluster.ET,T.sPtBalance);
		hA[138]->Fill(T.cluster.ET,T.sPtBalance2);
	  } else if(T.prMuTrack->charge()<0) {
		hA[131]->Fill(T.cluster.ET,T.sPtBalance);
		hA[139]->Fill(T.cluster.ET,T.sPtBalance2);
	  }

	  //remove ambiguous charges from BG treatment histos
	  if( fabs(hypCorr_p) > par_QET2PTlow && fabs(hypCorr_p) < par_QET2PThigh) { 
		for (int i=0; i<=20; i++) { // i index not currently used
		  for (int j=0; j<=80; j++) {
			float pTBal_cut = 5.+0.25*((float) j);
			if (T.sPtBalance2<pTBal_cut) { 
			  if (T.prMuTrack->charge() < 0) {
				hA[142+i]->Fill(T.cluster.ET,j);
			  } else if (T.prMuTrack->charge() > 0) {
				hA[163+i]->Fill(T.cluster.ET,j);
			  }
			}
		  }
		}

		//plots for backg sub yield
		if(T.sPtBalance2>par_ptBalance ) {
		  hA[136]->Fill(T.cluster.ET);//signal
		  hA[241]->Fill(T.prMuTrack->eta(),T.cluster.ET);
		  hA[62]->Fill(T.pointTower.iEta ,T.cluster.energy);
		  if (T.prMuTrack->charge() < 0) {
			hA[184+1]->Fill(T.cluster.ET);
		  } else if (T.prMuTrack->charge() > 0) {
			hA[184+2]->Fill(T.cluster.ET);
		  }
		} else {
		  hA[137]->Fill(T.cluster.ET);//background
		  if (T.prMuTrack->charge() < 0) {
			hA[184+5]->Fill(T.cluster.ET);
		  } else if (T.prMuTrack->charge() > 0) {
			hA[184+6]->Fill(T.cluster.ET);
		  }
		  hA[202]->Fill(T.cluster.position.PseudoRapidity(),T.prMuTrack->pt());
		  hA[204]->Fill(T.cluster.position.PseudoRapidity(),T.cluster.energy/T.prMuTrack->p().mag());
		}
	  }	


	  if(T.sPtBalance2>par_ptBalance && T.cluster.ET>par_highET){/***************************/
		printf("\n WWWWWWWWWWWWWWWWWWWWW  Barrel \n");
		wDisaply->exportEvent( "WB", V, T, iv);
		wEve->print();
	  }/***************************/


	  //put final W cut here
	  if(T.sPtBalance2<par_ptBalance)  continue;
	  hA[20]->Fill("noAway",1.0);
	  nNoAway++;

	  //****loop over branch with EEMC****  Feb 23:27:35, Jinlong Zhang
	  StJetVertex* jetVertex = mJetEvent->vertex(V.id);
	  assert(jetVertex->position().z() == V.z); //check that vert-z position match
	  int nJetsWE = jetVertex->numberOfJets();
	  for (int i_jet=0; i_jet< nJetsWE; i_jet++){//loop over jets
		StJetCandidate* jet = jetVertex->jet(i_jet);
		TVector3 jetVec; //vector for jet momentum
		jetVec.SetPtEtaPhi(jet->pt(),jet->eta(),jet->phi());
		if(jetVec.DeltaR(T.primP) > par_nearDeltaR) {
		  hA[126]->Fill(T.primP.Eta(), jet->eta());
		  hA[127]->Fill(T.primP.Phi(), jet->phi());
		  hA[128]->Fill(jet->pt());
		  hA[129]->Fill(jet->pt(),jetVec.DeltaPhi(T.primP));
		  hA[130]->Fill(jet->pt(),jet->eta());
		}
	  }
	  hA[125]->Fill(T.jetCount);

	  //::::::::::::::::::::::::::::::::::::::::::::::::
	  //:::::accepted W events for x-section :::::::::::
	  //::::::::::::::::::::::::::::::::::::::::::::::::

	  hA[113]->Fill( T.cluster.ET);//for Joe

	  hA[90]->Fill( T.cluster.ET); 
	  hA[92]->Fill( T.cluster.ET,T.glMuTrack->dEdx()*1e6); 
	  //hA[93]->Fill( T.cluster.ET,T.glMuTrack->dca(V.id).mag());
	  int k=0; if(T.prMuTrack->charge()<0) k=1;
	  hA[94+k]->Fill( T.cluster.ET,T.glMuTrack->dcaD());
	  // h95 used above

	  //plots to investigate east/west yield diff
	  hA[200]->Fill(T.cluster.position.PseudoRapidity(),T.cluster.ET);
	  hA[201]->Fill(T.cluster.position.PseudoRapidity(),T.prMuTrack->pt());
	  hA[203]->Fill(T.cluster.position.PseudoRapidity(),T.cluster.energy/T.prMuTrack->p().mag());
	  hA[205]->Fill(T.prMuTrack->lastPoint().pseudoRapidity(),T.prMuTrack->lastPoint().phi());

	  //Q/pT plot
	  hA[100]->Fill(T.cluster.ET,T.glMuTrack->charge()/T.glMuTrack->pt());
	  hA[101]->Fill(T.cluster.ET,T.prMuTrack->charge()/T.prMuTrack->pt());
	  hA[102]->Fill(T.cluster.ET,hypCorr_g);
	  hA[103]->Fill(T.cluster.ET,hypCorr_p);

	  //for each sector
	  int isec = WtpcFilter::getTpcSec(ro.phi(),ro.pseudoRapidity())-1;
	  hA[260+isec]->Fill(T.cluster.ET,T.glMuTrack->charge()/T.glMuTrack->pt());
	  hA[284+isec]->Fill(T.cluster.ET,T.prMuTrack->charge()/T.prMuTrack->pt());
	  hA[356+isec]->Fill(T.cluster.ET,hypCorr_p);
	  if(k==0) hA[308+isec]->Fill( T.cluster.ET,T.glMuTrack->dcaD());
	  else hA[332+isec]->Fill( T.cluster.ET,T.glMuTrack->dcaD());

	  if(T.cluster.ET<par_highET) continue;  // very likely Ws
	  hA[91]->Fill(T.cluster.position.PseudoRapidity(),T.cluster.position.Phi());
	  hA[96]->Fill(V.id);
	  hA[97]->Fill(V.funnyRank);
	  hA[98]->Fill(V.z);
	  hA[99]->Fill(T.prMuTrack->eta());
	  hA[191+k]->Fill(T.prMuTrack->eta(),T.cluster.ET);
	  hA[106]->Fill(wEve->zdcRate);

	  hA[20]->Fill("goldW",1.);
	  nGoldW++;
	  if(T.prMuTrack->charge()>0) nGoldWp++;
	  else if(T.prMuTrack->charge()<0) nGoldWn++;
	  hA[104]->Fill(wEve->time);

	  // free quark search
	  hA[105]->Fill(hypCorr_p,T.glMuTrack->dEdx()*1e6); 

	}// loop over tracks
  }// loop over vertices
  if(nNoNear>0) hA[0]->Fill("noNear",1.);
  if(nNoAway>0) hA[0]->Fill("noAway",1.);
  if(nEta1>0) hA[0]->Fill("eta1",1.);
  if(nGoldW>0) hA[0]->Fill("goldW",1.);
  if(nGoldWp>0) hA[0]->Fill("goldW+",1.);
  if(nGoldWn>0) hA[0]->Fill("goldW-",1.);

}


//________________________________________________
//________________________________________________
void
St2011WMaker::tag_Z_boson(){

  float par_jetPt=10.;
  float lowMass=70.; float highMass=140.;

  //form invariant mass from lepton candidate and jet
  for(uint iv=0;iv<wEve->vertex.size();iv++) {//vertex loop
	WeveVertex &V=wEve->vertex[iv];
	for(uint it=0;it<V.eleTrack.size();it++) {// select track
	  WeveEleTrack &T1=V.eleTrack[it];
	  if(T1.isMatch2Cl==false) continue;
	  assert(T1.cluster.nTower>0); // internal logical error
	  assert(T1.nearTotET>0); // internal logical error

	  //match lepton candidate with jet
	  TLorentzVector jetVec;

	  getJetEvent(); //check that jet and W event match

	  StJetVertex* jetVertex = mJetEvent->vertex(V.id);
	  int nJets = jetVertex->numberOfJets();
	  for (int i_jet=0; i_jet< nJets; i_jet++){//loop over jets
		StJetCandidate* jet = jetVertex->jet(i_jet);

		jetVec = jet->fourMomentum();
		if(jetVec.Pt()<par_jetPt) continue;//remove low pt jets

		//electron like cut on jets
		float maxCluster=0.; 
		int totTowers = jet->numberOfTowers();
		for(int itow=0;itow<totTowers;itow++){//loop over towers
		  StJetTower *tower = jet->tower(itow);
		  if(tower->detectorId()==13)//drop endcap towers
			continue;

		  int softId=tower->id();
		  //find highest 2x2 tower cluster in jet
		  TVector3 pos=positionBtow[softId-1]; int iEta,iPhi;
		  if( L2algoEtaPhi2IJ(pos.Eta(),pos.Phi(),iEta,iPhi)) 
			continue;
		  float cluster=maxBtow2x2(iEta,iPhi,V.z).ET;
		  if(cluster>maxCluster) maxCluster=cluster;
		}

		TVector3 jetVec3(jetVec.X(),jetVec.Y(),jetVec.Z());
		if(jetVec3.DeltaR(T1.primP)<par_nearDeltaR)
		  continue;//skip jets in candidate phi isolation'cone'

		//form invM
		float e1=T1.cluster.energy;
		TVector3 p1=T1.primP; p1.SetMag(e1);
		TLorentzVector ele1(p1,e1); //lepton candidate 4- mom
		TLorentzVector sum=ele1+jetVec;
		float invM=sqrt(sum*sum);
		if(maxCluster/jetVec3.Pt() < 0.5) continue;
		if(invM > lowMass && invM < highMass){
		  wEve->zTag=true;
		  //cout<<"tagged as Z mass = "<<invM<<endl;
		}
	  }
	}
  }

}

//________________________________________________
//________________________________________________
void
St2011WMaker::findPtBalance(){

  for(uint iv=0;iv<wEve->vertex.size();iv++) {
	WeveVertex &V=wEve->vertex[iv];
	for(uint it=0;it<V.eleTrack.size();it++) {
	  WeveEleTrack &T=V.eleTrack[it];

	  getJetEvent(); //check that jet and W event match

	  //****loop over branch with EEMC****
	  StJetVertex* jetVertex = mJetEvent->vertex(V.id);
	  assert(jetVertex);
	  assert(jetVertex->position().z() == V.z); //check that vert-z position match
	  int nJetsWE = jetVertex->numberOfJets();
	  int nJetsOutNearCone=0;  // add to count jet in signed pT; jinlong 2014/12/19
	  for (int i_jet=0; i_jet< nJetsWE; i_jet++){//loop over jets
		StJetCandidate* jet = jetVertex->jet(i_jet);
		TVector3 jetVec; //vector for jet momentum
		jetVec.SetPtEtaPhi(jet->pt(),jet->eta(),jet->phi());
		if(jetVec.DeltaR(T.primP) > par_nearDeltaR) {
		  T.ptBalance+=jetVec;
		  nJetsOutNearCone++; //  add to count jet in signed pT; jinlong 2014/12/19
		  hA[120]->Fill(T.primP.Eta(), jet->eta());
		  hA[121]->Fill(T.primP.Phi(), jet->phi());
		  hA[122]->Fill(jet->pt());
		  hA[123]->Fill(jet->pt(),jetVec.DeltaPhi(T.primP));
		  hA[124]->Fill(jet->pt(),jet->eta());
		}
	  }

	  hA[119]->Fill(nJetsOutNearCone);
	  T.jetCount = nJetsOutNearCone; //  add to count jet in signed pT; jinlong 2014/12/19 
	  TVector3 clustPt(T.primP.X(),T.primP.Y(),0);
	  clustPt.SetMag(T.cluster.ET);
	  T.ptBalance+=clustPt;
	  T.sPtBalance = T.ptBalance.Perp();
	  if(T.ptBalance.Dot(clustPt)<0) T.sPtBalance *=-1.;
	  T.sPtBalance2 = T.ptBalance.Dot(clustPt)/T.cluster.ET; //invariant

	  //****loop over branch without EEMC****
	  StJetVertex* jetVertex_noEEMC = mJetEvent_noEEMC->vertex(V.id);
	  assert(jetVertex_noEEMC->position().z() == V.z); //check that vert-z position match
	  int nJetsNE = jetVertex_noEEMC->numberOfJets();
	  for (int i_jet=0; i_jet< nJetsNE; i_jet++){//loop over jets
		StJetCandidate* jet = jetVertex_noEEMC->jet(i_jet);
		TVector3 jetVec; //vector for jet momentum
		jetVec.SetPtEtaPhi(jet->pt(),jet->eta(),jet->phi());
		if(jetVec.DeltaR(T.primP) > par_nearDeltaR)
		  T.ptBalance_noEEMC+=jetVec;
	  }
	  T.ptBalance_noEEMC+=clustPt;
	  T.sPtBalance_noEEMC = T.ptBalance_noEEMC.Perp();
	  if(T.ptBalance_noEEMC.Dot(clustPt)<0) T.sPtBalance_noEEMC *=-1.;
	  T.sPtBalance_noEEMC2 = T.ptBalance_noEEMC.Dot(clustPt)/T.cluster.ET; //invariant

	}// end of loop over tracks
  }// end of loop over vertices

}


//________________________________________________
//________________________________________________
void
St2011WMaker::findAwayJet(){
  // printf("\n******* find AwayJet() nVert=%d\n",wEve->vertex.size());
  //wEve->print();
  for(uint iv=0;iv<wEve->vertex.size();iv++) {
	WeveVertex &V=wEve->vertex[iv];
	for(uint it=0;it<V.eleTrack.size();it++) {
	  WeveEleTrack &T=V.eleTrack[it];

	  // .... sum opposite in phi EMC components
	  T.awayBtowET=sumBtowCone(V.z,-T.primP,1); // '1'= only cut on delta phi
	  T.awayEmcET=T.awayBtowET;
	  T.awayEtowET=sumEtowCone(V.z,-T.primP,1);
	  T.awayEmcET+=T.awayEtowET; 

	  //..... add TPC ET
	  if(mMuDstMaker) T.awayTpcPT=sumTpcCone(V.id,-T.primP,1,T.pointTower.id);
	  else T.awayTpcPT=sumTpcConeFromTree(iv,-T.primP,1,T.pointTower.id);
	  T.awayTotET=T.awayEmcET+T.awayTpcPT;
	  T.awayTotET_noEEMC=T.awayBtowET+T.awayTpcPT;
	  //printf("\n*** in   awayTpc=%.1f awayEmc=%.1f\n  ",T.awayTpcPT,T.awayEmcET); T.print(); 

	}// end of loop over tracks
  }// end of loop over vertices
}

//________________________________________________
//________________________________________________
void
St2011WMaker::findNearJet(){
  //printf("\n******* findNearJet() nVert=%d\n",wEve->vertex.size());

  for(uint iv=0;iv<wEve->vertex.size();iv++) {
	WeveVertex &V=wEve->vertex[iv];
	for(uint it=0;it<V.eleTrack.size();it++) {
	  WeveEleTrack &T=V.eleTrack[it];

	  // .... sum EMC-jet component
	  T.nearBtowET=sumBtowCone(V.z,T.primP,2); // '2'=2D cone
	  T.nearEmcET+=T.nearBtowET;
	  T.nearEtowET=sumEtowCone(V.z,T.primP,2);
	  T.nearEmcET+=T.nearEtowET; 
	  // .... sum TPC-near component
	  if(mMuDstMaker) T.nearTpcPT=sumTpcCone(V.id,T.primP,2,T.pointTower.id); // '2'=2D cone
	  else T.nearTpcPT=sumTpcConeFromTree(iv,T.primP,2,T.pointTower.id);
	  float nearSum=T.nearEmcET+T.nearTpcPT;

	  //fill histos separately for 2 types of events
	  if(T.pointTower.id>0) { //only barrel towers
		/* correct for double counting of electron track in near cone rarely primTrPT<10 GeV & globPT>10 - handle this here */
		if(T.primP.Pt()>par_trackPt) nearSum-=par_trackPt; 
		else  nearSum-=T.primP.Pt();
		T.nearTotET=nearSum;
		T.nearTotET_noEEMC=nearSum-T.nearEtowET; 
		float nearTotETfrac=T.cluster.ET/ T.nearTotET;
		float nearTotETfrac_noEEMC=T.cluster.ET/ T.nearTotET_noEEMC;

		//move requirement here for consistency, but now calc nearCone for all candidates
		if(T.isMatch2Cl==false) continue;

		hA[40]->Fill(T.nearEmcET);
		hA[41]->Fill(T.cluster.ET,T.nearEmcET-T.cluster.ET);
		hA[42]->Fill(nearTotETfrac);
		hA[131]->Fill(nearTotETfrac_noEEMC);
		hA[47]->Fill(T.nearTpcPT);
		hA[48]->Fill(T.nearEmcET,T.nearTpcPT);
		hA[49]->Fill(nearSum);
		hA[250]->Fill(T.cluster.ET,nearTotETfrac);

		// check east/west yield diff
		hA[210]->Fill(T.cluster.position.PseudoRapidity(),T.nearEtowET);
		if(T.cluster.position.PseudoRapidity()>0) hA[211]->Fill(T.cluster.position.Phi(),T.nearEtowET);
		else hA[212]->Fill(T.cluster.position.Phi(),T.nearEtowET);

	  }
	  else if(T.pointTower.id<0) { //only endcap towers
		/* correct for double counting of electron track in near cone rarely primTrPT<10 GeV & globPT>10 - handle this here */
		if(T.prMuTrack->flag()==301){ //short tracks aren't added to nearCone
		  if(T.primP.Pt()>parE_trackPt) nearSum-=parE_trackPt; 
		  else  nearSum-=T.primP.Pt();
		}
		T.nearTotET=nearSum;
		T.nearTotET_noEEMC=nearSum-T.nearEtowET;
		float nearTotETfrac=T.cluster.ET/ T.nearTotET;

		//move requirement here for consistency, but now calc nearCone for all candidates
		if(T.isMatch2Cl==false) continue;

		hE[40]->Fill(T.nearEmcET);
		hE[41]->Fill(T.cluster.ET,T.nearEmcET-T.cluster.ET);
		hE[42]->Fill(nearTotETfrac);
		hE[70]->Fill(T.cluster.ET/T.nearEmcET);
		hE[71]->Fill(T.cluster.ET/T.nearEtowET);
		hE[47]->Fill(T.nearTpcPT);
		hE[48]->Fill(T.nearEmcET,T.nearTpcPT);
		hE[49]->Fill(nearSum);
	  }  

	} // end of loop over tracks
  }// end of loop over vertices


}

//________________________________________________
//________________________________________________
float
St2011WMaker::sumBtowCone( float zVert,  TVector3 refAxis, int flag){
  /* flag=1 : only delta phi cut;  flag=2 use 2D cut */
  assert(flag==1 || flag==2);
  double ptSum=0;

  //.... process BTOW hits
  for(int i=0;i< mxBtow;i++) {
	float ene=wEve->bemc.eneTile[kBTow][i];
	if(ene<=0) continue;
	TVector3 primP=positionBtow[i]-TVector3(0,0,zVert);
	primP.SetMag(ene); // it is 3D momentum in the event ref frame
	if(flag==1) {
	  float deltaPhi=refAxis.DeltaPhi(primP);
	  if(fabs(deltaPhi)> par_awayDeltaPhi) continue;
	}
	if(flag==2) {
	  float deltaR=refAxis.DeltaR(primP);
	  if(deltaR> par_nearDeltaR) continue;
	}
	ptSum+=primP.Perp();
  }

  return ptSum;
}

//________________________________________________
//________________________________________________
float
St2011WMaker::sumTpcConeFromTree(int vertID, TVector3 refAxis, int flag,int pointTowId){

  // flag=2 use 2D cut, 1= only delta phi

  assert(vertID>=0);
  assert(vertID<(int)wEve->vertex.size());

  double ptSum=0;
  WeveVertex &V=wEve->vertex[vertID];
  for(uint it=0;it<V.prTrList.size();it++){
	StMuTrack *prTr=V.prTrList[it];
	if(prTr->flag()<=0) continue;
	if(prTr->flag()!=301 && pointTowId>0) continue;// TPC-only regular tracks for barrel candidate
	if(prTr->flag()!=301 && pointTowId<0) continue;// TPC-only regular tracks for endcap candidate 
	float hitFrac=1.*prTr->nHitsFit()/prTr->nHitsPoss();
	if(hitFrac<par_nHitFrac) continue;
	StThreeVectorF prPvect=prTr->p();
	TVector3 primP=TVector3(prPvect.x(),prPvect.y(),prPvect.z());
	// printf(" prTrID=%4d  prTrEta=%.3f prTrPhi/deg=%.1f prPT=%.1f  nFitPts=%d\n", prTr->id(),prTr->eta(),prTr->phi()/3.1416*180.,prTr->pt(),prTr->nHitsFit());
	if(flag==1) {
	  float deltaPhi=refAxis.DeltaPhi(primP);
	  if(fabs(deltaPhi)> par_awayDeltaPhi) continue;
	}
	if(flag==2) {
	  float deltaR=refAxis.DeltaR(primP);
	  if(deltaR>par_nearDeltaR) continue;

	}
	float pT=prTr->pt();
	//    printf(" passed pt=%.1f\n",pT);

	//separate quench for barrel and endcap candidates
	if(pT>par_trackPt && pointTowId>0) ptSum+=par_trackPt;
	else if(pT>parE_trackPt && pointTowId<0) ptSum+=parE_trackPt;
	else  ptSum+=pT;
  }
  return ptSum;
}


// ************* Barrel Code ************ //
// ************************************** //

//________________________________________________
//________________________________________________
int
St2011WMaker::extendTrack2Barrel(){// return # of extended tracks
  //printf("******* extendTracks() nVert=%d\n",wEve->vertex.size());
  if(!wEve->l2bitET) return 0; //fire barrel trigger

  int nTrB=0;
  for(uint iv=0;iv<wEve->vertex.size();iv++) {
	WeveVertex &V=wEve->vertex[iv];
	if(V.rank<0) continue; //remove vertex for endcap algo only
	for(uint it=0;it<V.eleTrack.size();it++) {
	  WeveEleTrack &T=V.eleTrack[it];
	  if(T.prMuTrack->flag()!=301) continue; //remove track for endcap algo only

	  //do eta sorting at track level (tree analysis)
	  if(T.primP.Eta() < par_leptonEtaLow || T.primP.Eta() > par_leptonEtaHigh) continue;

	  //.... extrapolate track to the barrel @ R=entrance....
	  const StPhysicalHelixD TrkHlx=T.prMuTrack->outerHelix();
	  float Rcylinder= mBtowGeom->Radius();
	  pairD  d2;
	  d2 = TrkHlx.pathLength(Rcylinder);
	  //printf(" R=%.1f path 1=%f, 2=%f, period=%f, R=%f\n",Rctb,d2.first ,d2.second,TrkHlx.period(),1./TrkHlx.curvature());

	  // assert(d2.first<0); // propagate backwards
	  // assert(d2.second>0); // propagate forwards
	  if(d2.first>=0 || d2.second<=0) {
		LOG_WARN<< Form("MatchTrk , unexpected solution for track crossing CTB\n  d2.firts=%f, second=%f, swap them",  d2.first, d2.second)<<endm;
		float xx=d2.first;
		d2.first=d2.second;
		d2.second=xx;
	  }

	  // extrapolate track to cylinder
	  StThreeVectorD posR = TrkHlx.at(d2.second);
	  //printf(" punch2 x,y,z=%.1f, %.1f, %.1f, Rxy=%.1f\n",posCTB.x(),posCTB.y(),posCTB.z(),xmagn);
	  float etaF=posR.pseudoRapidity();
	  float phiF=posR.phi();
	  int iEta,iPhi;
	  if( L2algoEtaPhi2IJ(etaF, phiF,iEta,iPhi)) continue;
	  nTrB++;
	  hA[20]->Fill("@B",1.);
	  //printf(" phi=%.0f deg,  eta=%.2f, iEta=%d, iPhi=%d\n",posCTB.phi()/3.1416*180.,posCTB. pseudoRapidity(),iEta, iPhi);
	  int twID= mapBtowIJ2ID[ iEta+ iPhi*mxBTetaBin];
	  // printf("hit Tower ID=%d\n",twID);

	  T.pointTower.id=twID;
	  T.pointTower.R=TVector3(posR.x(),posR.y(),posR.z());
	  T.pointTower.iEta=iEta;
	  T.pointTower.iPhi=iPhi;
	  //T.print();
	}
  }// end of loop over vertices

  if(nTrB<=0) return -1;
  hA[0]->Fill("TrB",1.0);
  return 0;

}

//________________________________________________
//________________________________________________
int
St2011WMaker::matchTrack2BtowCluster(){
  // printf("******* matchCluster() nVert=%d\n",wEve->vertex.size());
  int nTr=0;
  float Rcylinder= mBtowGeom->Radius();
  for(uint iv=0;iv<wEve->vertex.size();iv++) {
	WeveVertex &V=wEve->vertex[iv];
	float zVert=V.z;
	for(uint it=0;it<V.eleTrack.size();it++) {
	  WeveEleTrack &T=V.eleTrack[it];
	  if(T.pointTower.id<=0) continue; //skip endcap towers

	  float trackPT=T.prMuTrack->momentum().perp();
	  T.cluster=maxBtow2x2( T.pointTower.iEta, T.pointTower.iPhi,zVert);      
	  hA[33]->Fill( T.cluster.ET);
	  hA[34]->Fill(T.cluster.adcSum,trackPT);
	  hA[110]->Fill( T.cluster.ET);

	  // ........compute surroinding cluster energy
	  int iEta=T.cluster.iEta;
	  int iPhi=T.cluster.iPhi;
	  T.cl4x4=sumBtowPatch(iEta-1,iPhi-1,4,4,zVert); // needed for lumi monitor

	  if (T.cluster.ET <par_clustET) continue; // too low energy
	  hA[20]->Fill("CL",1.);

	  hA[206]->Fill(T.cluster.position.PseudoRapidity(),T.cluster.ET);

	  hA[37]->Fill( T.cl4x4.ET);
	  hA[38]->Fill(T.cluster.energy, T.cl4x4.energy-T.cluster.energy);

	  float frac24=T.cluster.ET/(T.cl4x4.ET);
	  hA[39]->Fill(frac24);
	  if(frac24<par_clustFrac24) continue;

	  hA[20]->Fill("fr24",1.);

	  //.. spacial separation (track - cluster)
	  TVector3 D=T.pointTower.R-T.cluster.position;

	  hA[43]->Fill( T.cluster.energy,D.Mag());
	  hA[44]->Fill( T.cluster.position.z(),D.z());
	  float delPhi=T.pointTower.R.DeltaPhi(T.cluster.position);
	  //   printf("aaa %f %f %f   phi=%f\n",D.x(),D.y(),D.z(),delPhi);
	  hA[45]->Fill( T.cluster.energy,Rcylinder*delPhi);// wrong?
	  hA[46]->Fill( D.Mag());
	  hA[199]->Fill(T.cluster.position.PseudoRapidity(),D.Mag());
	  hA[207]->Fill(T.cluster.position.PseudoRapidity(),T.cluster.ET);

	  if(D.Mag()>par_delR3D) continue; 
	  T.isMatch2Cl=true; // cluster is matched to TPC track
	  hA[20]->Fill("#Delta R",1.);
	  hA[111]->Fill( T.cluster.ET);

	  hA[208]->Fill(T.cluster.position.PseudoRapidity(),T.cluster.ET);

	  nTr++;
	}// end of one vertex
  }// end of vertex loop

  if(nTr<=0) return -1; 
  hA[0]->Fill("Tr2Cl",1.0);
  return 0;

}

//________________________________________________
//________________________________________________ 
WeveCluster 
St2011WMaker::maxBtow2x2(int iEta, int iPhi, float zVert){
  //printf("   maxBtow2x2  seed iEta=%d iPhi=%d \n",iEta, iPhi);
  const int L=2; // size of the summed square 

  WeveCluster maxCL;
  // just 4 cases of 2x2 clusters
  float maxET=0;
  int I0=iEta-1;
  int J0=iPhi-1;
  for(int I=I0;I<=I0+1;I++){
	for(int J=J0;J<=J0+1;J++) {
	  WeveCluster CL=sumBtowPatch(I,J,L,L,zVert);
	  if(maxET>CL.ET) continue;
	  maxET=CL.ET;
	  maxCL=CL;
	  // printf("   newMaxETSum=%.1f iEta=%d iPhi=%d \n",maxET, I,J);
	}
  }// 4 combinations done
  //printf(" final inpEve=%d SumET2x2=%.1f \n",nInpEve,maxET);
  return maxCL;  
}


//________________________________________________
//________________________________________________
WeveCluster
St2011WMaker::sumBtowPatch(int iEta, int iPhi, int Leta,int  Lphi, float zVert){
  //printf("  eveID=%d btowSquare seed iEta=%d[+%d] iPhi=%d[+%d] zVert=%.0f \n",wEve->id,iEta,Leta, iPhi,Lphi,zVert);
  WeveCluster CL; // object is small, not to much overhead in creating it
  CL.iEta=iEta;
  CL.iPhi=iPhi;
  TVector3 R; 
  double sumW=0;
  float Rcylinder= mBtowGeom->Radius(), Rcylinder2=Rcylinder*Rcylinder;
  for(int i=iEta; i<iEta+Leta;i++){// trim in eta-direction
	if(i<0) continue;
	if(i>=mxBTetaBin) continue;
	for(int j=iPhi;j<iPhi+Lphi;j++) {// wrap up in the phi-direction
	  int jj=(j+mxBTphiBin)%mxBTphiBin;// keep it always positive
	  //if(L<5) printf("n=%2d  i=%d jj=%d\n",CL.nTower,i,jj);
	  int softID= mapBtowIJ2ID[ i+ jj*mxBTetaBin];
	  float ene= wEve->bemc.eneTile[kBTow][softID-1];
	  if(ene<=0) continue; // skip towers w/o energy
	  float adc= wEve->bemc.adcTile[kBTow][softID-1];
	  float delZ=positionBtow[softID-1].z()-zVert;
	  float e2et=Rcylinder/sqrt(Rcylinder2+delZ*delZ);
	  float ET=ene*e2et;
	  float logET=log10(ET+0.5);
	  CL.nTower++;
	  CL.energy+=ene;
	  CL.ET+=ET;
	  CL.adcSum+=adc;
	  if(logET>0) {
		R+=logET*positionBtow[softID-1];
		sumW+=logET;
	  }
	  //if(Leta==2) printf("      iEta=%d iPhi=%d  ET=%.1f  ene=%.1f   sum=%.1f logET=%f sumW=%f\n",i,j,ET,ene,CL.energy,logET,sumW); 
	}
	// printf(" end btowSquare: iEta=%d  nTw=%d, ET=%.1f adc=%.1f\n",i,CL.nTower,CL.ET,CL.adcSum);
	if(sumW>0) {
	  CL.position=1./sumW*R; // weighted cluster position
	} else {
	  CL.position=TVector3(0,0,999);
	} 
  }
  return CL;
}


// $Log: St2011W_algo.cxx,v $
// Revision 1.22  2016/01/08 02:08:49  jlzhang
// added couples histograms and fixed a small bug
//
// Revision 1.21  2013/09/13 19:33:13  stevens4
// Updates to code for combined 2011+2012 result presented to spin PWG 9.12.13
//
// Revision 1.20  2012/10/05 17:53:53  balewski
// added correlation plots for reco Q in Z, W algos
//
// Revision 1.19  2012/09/28 16:00:42  stevens4
// add Q*ET/PT requirement to WB histos used for background estimation to be consistent with spin sorting
//
// Revision 1.18  2012/09/26 14:20:59  stevens4
// use PtBal cos(phi) for WB and WE algos and use Q*ET/PT for barrel charge sign
//
// Revision 1.17  2012/09/18 22:30:18  stevens4
// change to new jet tree format with access to all rank>0 vertices
//
// Revision 1.16  2012/09/18 21:10:07  stevens4
// Include all rank>0 vertex again (new jet format coming next), and remove rank<0 endcap vertices.
//
// Revision 1.15  2012/09/17 03:29:30  stevens4
// Updates to Endcap algo and Q*ET/PT charge separation
//
// Revision 1.14  2012/08/31 20:10:52  stevens4
// switch to second EEMC background using both isolation and sPt-Bal (for mirror symmetry (also adjust eta binning)
//
// Revision 1.13  2012/08/28 14:28:27  stevens4
// add histos for barrel and endcap algos
//
// Revision 1.12  2012/08/21 21:28:22  stevens4
// Add spin sorting for endcap Ws
//
// Revision 1.11  2012/08/21 18:29:16  stevens4
// Updates to endcap W selection using ESMD strip ratio
//
// Revision 1.10  2012/08/07 21:06:38  stevens4
// update to tree analysis to produce independent histos in a TDirectory for each eta-bin
//
// Revision 1.9  2012/07/13 20:53:16  stevens4
// Add filling of empty events in W tree
// Minor modifications to histograms
//
// Revision 1.8  2012/07/06 17:47:02  stevens4
// Updates for tree reader
//
// Revision 1.7  2012/07/05 19:03:54  stevens4
// preset bin labels for TPC histos to preven merge warning
//
// Revision 1.6  2012/06/29 21:20:08  stevens4
// *** empty log message ***
//
// Revision 1.5  2012/06/25 20:53:24  stevens4
// algo and histo cleanup
//
// Revision 1.4  2012/06/18 18:28:01  stevens4
// Updates for Run 9+11+12 AL analysis
//
// Revision 1.3  2011/02/25 06:03:47  stevens4
// addes some histos and enabled running on MC
//
// Revision 1.2  2011/02/17 04:16:22  stevens4
// move sector dependent track QA cuts before track pt>10 cut and lower par_clustET and par_ptBalance thresholds to 14 GeV
//
// Revision 1.1  2011/02/10 20:33:23  balewski
// start
//
