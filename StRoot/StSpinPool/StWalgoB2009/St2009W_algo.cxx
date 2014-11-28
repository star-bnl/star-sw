// $Id: St2009W_algo.cxx,v 1.25 2011/09/14 14:23:20 stevens4 Exp $
//
//*-- Author : Jan Balewski, MIT
//*-- Author for Endcap: Justin Stevens, IUCF

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "WeventDisplay.h"
#include "StSpinPool/StJets/StJet.h"
#include "StSpinPool/StJets/TowerToJetIndex.h"

//muDst needed for zdcRate
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include "St2009WMaker.h"

//________________________________________________
//________________________________________________
void
St2009WMaker::find_W_boson(){
  
  // printf("========= find_W_boson() \n");
  int nGoldW=0;
  //remove events tagged as Zs
  if(wEve.zTag) return;

  // search for  Ws ............
  for(uint iv=0;iv<wEve.vertex.size();iv++) {
    WeveVertex &V=wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error
      
      //make cut on lepton |eta| for cross section 
      if(fabs(T.primP.Eta()) > par_leptonEta) continue;      
      hA[0]->Fill("eta1",1.);

      //signal plots w/o EEMC in veto
      if(T.cluster.ET/T.nearTotET_noEEMC>par_nearTotEtFrac){
	if(T.awayTotET_noEEMC < 8)//old awayside pt cut
	  hA[141]->Fill(T.cluster.ET);
	if(T.sPtBalance_noEEMC>par_ptBalance ) {//only signed ptBalance cut 
	  hA[140]->Fill(T.cluster.ET);
          if (T.prMuTrack->charge() < 0) {
            hA[184+3]->Fill(T.cluster.ET);
	    hA[200+3]->Fill(T.primP.Eta(),T.cluster.ET);
	    hA[280+3]->Fill(abs(T.primP.Eta()),T.cluster.ET);
          } else if (T.prMuTrack->charge() > 0) {
            hA[184+4]->Fill(T.cluster.ET);
	    hA[200+4]->Fill(T.primP.Eta(),T.cluster.ET);
	    hA[280+4]->Fill(abs(T.primP.Eta()),T.cluster.ET);
          }
        }
      }

      if(T.cluster.ET /T.nearTotET< par_nearTotEtFrac) continue; // too large nearET

      hA[20]->Fill("noNear",1.);
      hA[113]->Fill( T.cluster.ET); // for Joe
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

      int i=0;
      //background variations for systematic
      for (int j=0; j<=20; j++) { //loop over possible sPtBal cuts
	float pTBal_cut = 5.+((float) j);
	if (T.sPtBalance<pTBal_cut) { //background
	  if (T.prMuTrack->charge() < 0) {
	    hA[142+i]->Fill(T.cluster.ET,j);
	    hA[210+j]->Fill(T.primP.Eta(),T.cluster.ET);
	    hA[290+j]->Fill(abs(T.primP.Eta()),T.cluster.ET);
	  } else if (T.prMuTrack->charge() > 0) {
	    hA[163+i]->Fill(T.cluster.ET,j);
	    hA[231+j]->Fill(T.primP.Eta(),T.cluster.ET);
	    hA[311+j]->Fill(abs(T.primP.Eta()),T.cluster.ET);
	  }
	}
      }
      
      //plots for backg sub yield
      if(T.sPtBalance>par_ptBalance ) {
        hA[136]->Fill(T.cluster.ET);//signal
        hA[62]->Fill(T.pointTower.iEta ,T.cluster.energy);
        if (T.prMuTrack->charge() < 0) {
          hA[184+1]->Fill(T.cluster.ET);
	  hA[200+1]->Fill(T.primP.Eta(),T.cluster.ET);
	  hA[280+1]->Fill(abs(T.primP.Eta()),T.cluster.ET);
        } else if (T.prMuTrack->charge() > 0) {
          hA[184+2]->Fill(T.cluster.ET);
	  hA[200+2]->Fill(T.primP.Eta(),T.cluster.ET);
	  hA[280+2]->Fill(abs(T.primP.Eta()),T.cluster.ET);
        }
      } else {
        hA[137]->Fill(T.cluster.ET);//background
        if (T.prMuTrack->charge() < 0) {
          hA[184+5]->Fill(T.cluster.ET);
	  hA[200+5]->Fill(T.primP.Eta(),T.cluster.ET);
	  hA[280+5]->Fill(abs(T.primP.Eta()),T.cluster.ET);
        } else if (T.prMuTrack->charge() > 0) {
          hA[184+6]->Fill(T.cluster.ET);
	  hA[200+6]->Fill(T.primP.Eta(),T.cluster.ET);
	  hA[280+6]->Fill(abs(T.primP.Eta()),T.cluster.ET);
        }
      }

      //plots for backg sub yield (old awayTot cut DNP)
      if(T.awayTotET < 8)
        hA[138]->Fill(T.cluster.ET);//old signal
      else 
        hA[139]->Fill(T.cluster.ET);//old background
      
      if(0){/***************************/
	  printf("\n WWWWWWWWWWWWWWWWWWWWW\n");
	  wDisaply->exportEvent( "W", V, T);
	  wEve.print();
	}/***************************/
 
	
      //put final W cut here
      if(T.sPtBalance<par_ptBalance)  continue;
      //::::::::::::::::::::::::::::::::::::::::::::::::
      //:::::accepted W events for x-section :::::::::::
      //::::::::::::::::::::::::::::::::::::::::::::::::
      wEve.wTag=true;

      hA[20]->Fill("noAway",1.0);  
      hA[114]->Fill( T.cluster.ET);//for Joe

      hA[90]->Fill( T.cluster.ET); 
      hA[92]->Fill( T.cluster.ET,T.glMuTrack->dEdx()*1e6); 
      hA[93]->Fill( T.cluster.ET,T.glMuTrack->dca().mag());
      int k=0; if(T.prMuTrack->charge()<0) k=1;
      hA[94+k]->Fill( T.cluster.ET,T.glMuTrack->dcaD());
      // h95 used above

 
      if(T.cluster.ET<par_highET) continue;  // very likely Ws
      hA[91]->Fill(T.cluster.position.PseudoRapidity(),T.cluster.position.Phi());
      hA[96]->Fill(V.id);
      hA[97]->Fill(V.funnyRank);
      hA[98]->Fill(V.z);
      hA[99]->Fill( T.prMuTrack->eta());
      hA[20]->Fill("goldW",1.);
      nGoldW++;

      //some final plots for comparing to embedding
      float zdcRate=mMuDstMaker->muDst()->event()->runInfo().zdcCoincidenceRate();
      hA[260]->Fill(zdcRate,wEve.bx7);
      hA[261]->Fill(zdcRate,wEve.vertex.size());
      hA[262]->Fill(zdcRate,T.prMuTrack->nHitsFit());
      float hitFrac=1.*T.prMuTrack->nHitsFit()/T.prMuTrack->nHitsPoss();
      hA[263]->Fill(zdcRate,hitFrac);
      hA[264]->Fill(zdcRate,T.prMuTrack->globalTrack()->lastPoint().perp());
      hA[265]->Fill(zdcRate,T.prMuTrack->globalTrack()->firstPoint().perp());
      hA[266]->Fill(zdcRate,T.prMuTrack->pt());
      hA[267]->Fill(zdcRate,T.prMuTrack->globalTrack()->chi2());
      hA[268]->Fill(zdcRate,1./T.prMuTrack->pt());
      hA[269]->Fill(zdcRate,T.glMuTrack->dca().mag());
      hA[270]->Fill(zdcRate,T.glMuTrack->dcaD());
      hA[271]->Fill(zdcRate,T.glMuTrack->dcaZ());

      if(V.funnyRank<10) {
	hA[272]->Fill(zdcRate,T.glMuTrack->pt());
	hA[273]->Fill(zdcRate,1./T.glMuTrack->pt());
      }

    }// loop over tracks
  }// loop over vertices
  if(nGoldW>0)
    hA[0]->Fill("goldW",1.);

}


//________________________________________________
//________________________________________________
void
St2009WMaker::tag_Z_boson(){

  float par_jetPt=10.;
  float lowMass=70.; float highMass=140.;
  mJets = getJets("ConeJets12_100"); //select specific jet-type

  //form invariant mass from lepton candidate and jet
  for(uint iv=0;iv<wEve.vertex.size();iv++) {//vertex loop
    WeveVertex &V=wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {// select track
      WeveEleTrack &T1=V.eleTrack[it];
      if(T1.isMatch2Cl==false) continue;
      assert(T1.cluster.nTower>0); // internal logical error
      assert(T1.nearTotET>0); // internal logical error
      if(T1.cluster.ET/T1.nearTotET< par_nearTotEtFrac) continue; // too large nearET

      //match lepton candidate with jet
      TLorentzVector jetVec;
      for (int i_jet=0; i_jet< nJets; i_jet++){//jet loop
        jetVec = *((StJet*)mJets->At(i_jet));
        if(jetVec.Pt()<par_jetPt) continue;//remove low pt jets
	
	//electron like cut on jets
        StJet* jet = getJet(i_jet);  float maxCluster=0.; 
        int totTowers=jet->nBtowers+jet->nEtowers;
        for(int itow=0;itow<totTowers;itow++){//loop over towers
	  if(jet->tower(itow)->detectorId()==13)//drop endcap towers
	    continue;
          
	  int softId=jet->tower(itow)->towerId();
          //find highest 2x2 BTOW cluster in jet
          TVector3 pos=positionBtow[softId-1]; int iEta,iPhi;
          if( L2algoEtaPhi2IJ(pos.Eta(),pos.Phi(),iEta,iPhi)) 
	    continue;
	  float cluster=maxBtow2x2(iEta,iPhi,jet->zVertex).ET;
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
	if(maxCluster/jet->jetPt < 0.5) continue;
	if(invM > lowMass && invM < highMass)
          wEve.zTag=true;
      }
    }
  }
  
}

//________________________________________________
//________________________________________________
void
St2009WMaker::findPtBalance(){

  for(uint iv=0;iv<wEve.vertex.size();iv++) {
    WeveVertex &V=wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;

      //****loop over branch with EEMC****
      mJets = getJets(mJetTreeBranch);
      int nJetsWE=nJets;
      for (int i_jet=0; i_jet< nJetsWE; i_jet++){//loop over jets
	StJet* jet = getJet(i_jet);
	TVector3 jetVec; //vector for jet momentum
	//vary neutral and charged et in jets for systematic
	float neutral=jet->neutralFraction()*jet->Pt();
	float charged=jet->chargedFraction()*jet->Pt();
	neutral=neutral*par_mcJetNeutScale;
	charged=charged*par_mcJetChrgScale;
	float sum=neutral+charged;
	jetVec.SetPtEtaPhi(sum,jet->Eta(),jet->Phi());
	if(jetVec.DeltaR(T.primP) > par_nearDeltaR)
              T.ptBalance+=jetVec;
      }
      TVector3 clustPt(T.primP.X(),T.primP.Y(),0);
      clustPt.SetMag(T.cluster.ET);
      T.ptBalance+=clustPt;
      T.sPtBalance = T.ptBalance.Perp();
      if(T.ptBalance.Dot(clustPt)<0) T.sPtBalance *=-1.;
      
      //****loop over branch without EEMC****
      mJets = getJets(mJetTreeBranch_noEEMC);
      int nJetsNE=nJets;
      TVector3 highJet; //zero out highJet 
      for (int i_jet=0; i_jet< nJetsNE; i_jet++){//loop over jets
	StJet* jet = getJet(i_jet);
	TVector3 jetVec; //vector for jet momentum
	//vary neutral and charged et in jets for systematic
	float neutral=jet->neutralFraction()*jet->Pt();
	float charged=jet->chargedFraction()*jet->Pt();
	neutral=neutral*par_mcJetNeutScale;
	charged=charged*par_mcJetChrgScale;
	float sum=neutral+charged;
	jetVec.SetPtEtaPhi(sum,jet->Eta(),jet->Phi());
	if(jetVec.DeltaR(T.primP) > par_nearDeltaR)
	  T.ptBalance_noEEMC+=jetVec;
      }
      T.ptBalance_noEEMC+=clustPt;
      T.sPtBalance_noEEMC = T.ptBalance_noEEMC.Perp();
      if(T.ptBalance_noEEMC.Dot(clustPt)<0) T.sPtBalance_noEEMC *=-1.;

    }// end of loop over tracks
  }// end of loop over vertices
  
}


//________________________________________________
//________________________________________________
void
St2009WMaker::findAwayJet(){
  // printf("\n******* find AwayJet() nVert=%d\n",wEve.vertex.size());
  //wEve.print();
  for(uint iv=0;iv<wEve.vertex.size();iv++) {
    WeveVertex &V=wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;
      
      // .... sum opposite in phi EMC components
      T.awayBtowET=sumBtowCone(V.z,-T.primP,1,T.awayNTow); // '1'= only cut on delta phi
      T.awayEmcET=T.awayBtowET;
      T.awayEtowET=sumEtowCone(V.z,-T.primP,1,T.awayNTow);
      if(par_useEtow >= 2) T.awayEmcET+=T.awayEtowET;

      //..... add TPC ET
      T.awayTpcPT=sumTpcCone(V.id,-T.primP,1,T.awayNTr);
      T.awayTotET=T.awayEmcET+T.awayTpcPT;
      T.awayTotET_noEEMC=T.awayBtowET+T.awayTpcPT;
      //printf("\n*** in   awayTpc=%.1f awayEmc=%.1f\n  ",T.awayTpcPT,T.awayEmcET); T.print(); 
    }// end of loop over tracks
  }// end of loop over vertices
}

//________________________________________________
//________________________________________________
void
St2009WMaker::findNearJet(){
  //printf("\n******* findNearJet() nVert=%d\n",wEve.vertex.size());

  for(uint iv=0;iv<wEve.vertex.size();iv++) {
    WeveVertex &V=wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;

       // .... sum EMC-jet component
      T.nearBtowET=sumBtowCone(V.z,T.primP,2,T.nearNTow); // '2'=2D cone
      T.nearEmcET+=T.nearBtowET;
      T.nearEtowET=sumEtowCone(V.z,T.primP,2,T.nearNTow);
      if(par_useEtow >= 3) T.nearEmcET+=T.nearEtowET;
      // .... sum TPC-near component
      T.nearTpcPT=sumTpcCone(V.id,T.primP,2,T.nearNTr); // '2'=2D cone
      hA[47]->Fill(T.nearTpcPT);
      hA[48]->Fill(T.nearEmcET,T.nearTpcPT);
      float nearSum=T.nearEmcET+T.nearTpcPT;
      
      /* correct for double counting of electron track in near cone
	 rarely primTrPT<10 GeV & globPT>10 - handle this here */
      if(T.primP.Pt()>par_trackPt) nearSum-=par_trackPt; 
      else  nearSum-=T.primP.Pt();
      T.nearTotET=nearSum;
      T.nearTotET_noEEMC=nearSum-T.nearEtowET;

      hA[49]->Fill(nearSum);

      // printf("\n*** in nearCone T.print\n  "); T.print();
      hA[40]->Fill( T.nearEmcET);
      hA[41]->Fill( T.cluster.ET,T.nearEmcET-T.cluster.ET);
      float nearTotETfrac=T.cluster.ET/ T.nearTotET;
      hA[42]->Fill(nearTotETfrac);
      hA[192]->Fill(T.cluster.ET,nearTotETfrac);
  
    } // end of loop over tracks
  }// end of loop over vertices
 

}


//________________________________________________
//________________________________________________
int
St2009WMaker::matchTrack2Cluster(){
  // printf("******* matchCluster() nVert=%d\n",wEve.vertex.size());
  //wEve.print();
  int nTr=0;
  float Rcylinder= mBtowGeom->Radius();
  for(uint iv=0;iv<wEve.vertex.size();iv++) {
    WeveVertex &V=wEve.vertex[iv];
    float zVert=V.z;
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      T.isMatch2Cl=false; // just in case (was already set in constructor)
      if(T.pointTower.id==0) continue;
      
      float trackPT=T.prMuTrack->momentum().perp();
      T.cluster=maxBtow2x2( T.pointTower.iEta, T.pointTower.iPhi,zVert);      
      hA[33]->Fill( T.cluster.ET);
      hA[34]->Fill(T.cluster.adcSum,trackPT);
      hA[110]->Fill( T.cluster.ET);
      if (T.cluster.ET <par_clustET) continue; // too low energy
      hA[20]->Fill("CL",1.);

      //.. spacial separation (track - cluster)
      TVector3 D=T.pointTower.R-T.cluster.position;
      hA[43]->Fill( T.cluster.energy,D.Mag());
      hA[44]->Fill( T.cluster.position.z(),D.z());
      float delPhi=T.pointTower.R.DeltaPhi(T.cluster.position);
      //   printf("aaa %f %f %f   phi=%f\n",D.x(),D.y(),D.z(),delPhi);
      hA[45]->Fill( T.cluster.energy,Rcylinder*delPhi);// wrong?
      hA[46]->Fill( D.Mag());

      if(D.Mag()>par_delR3D) continue; 
      hA[20]->Fill("#Delta R",1.);
      hA[111]->Fill( T.cluster.ET);

      // ........compute surroinding cluster energy
      int iEta=T.cluster.iEta;
      int iPhi=T.cluster.iPhi;
      T.cl4x4=sumBtowPatch(iEta-1,iPhi-1,4,4,zVert); // needed for lumi monitor
      hA[37]->Fill( T.cl4x4.ET);
      hA[38]->Fill(T.cluster.energy, T.cl4x4.energy-T.cluster.energy);
      
      //sum tpc PT  near lepton candidate track
      T.smallNearTpcPT=sumTpcCone(V.id,T.primP,3,T.smallNearNTr);
      hA[56]->Fill(T.smallNearTpcPT);
      T.smallNearTpcPT-=par_trackPt;

      float frac24=T.cluster.ET/(T.cl4x4.ET);
      hA[39]->Fill(frac24);
      hA[191]->Fill(T.cluster.ET,frac24);
      if(frac24<par_clustFrac24) continue;
      T.isMatch2Cl=true; // cluster is matched to TPC track

      hA[20]->Fill("fr24",1.);
      hA[112]->Fill( T.cluster.ET);
      
      nTr++;
    }// end of one vertex
  }// end of vertex loop

  if(nTr<=0) return -1; // cancel event w/o good tracks
  hA[0]->Fill("Tr2Cl",1.0);
  return 0;

}

//________________________________________________
//________________________________________________ 
WeveCluster 
St2009WMaker::maxBtow2x2(int iEta, int iPhi, float zVert){
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
St2009WMaker::sumBtowPatch(int iEta, int iPhi, int Leta,int  Lphi, float zVert){
  //printf("  eveID=%d btowSquare seed iEta=%d[+%d] iPhi=%d[+%d] zVert=%.0f \n",wEve.id,iEta,Leta, iPhi,Lphi,zVert);
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
      float ene= wEve.bemc.eneTile[kBTow][softID-1];
      if(ene<=0) continue; // skip towers w/o energy
      float adc= wEve.bemc.adcTile[kBTow][softID-1];
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

//________________________________________________
//________________________________________________
int
St2009WMaker::extendTrack2Barrel(){// return # of extended tracks
  //wEve.print();
  //printf("******* extendTracks() nVert=%d\n",wEve.vertex.size());
  int nTrB=0; 
  for(uint iv=0;iv<wEve.vertex.size();iv++) {
    WeveVertex &V=wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
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
    }
  }// end of loop over vertices

  if(nTrB<=0) return -1; // cancel event w/o good tracks
  hA[0]->Fill("TrB",1.0);
  return 0;

}


//________________________________________________
//________________________________________________
//________________________________________________
float
St2009WMaker::sumBtowCone( float zVert,  TVector3 refAxis, int flag, int &nTow){ 
  /* flag=1 : only delta phi cut;  flag=2 use 2D cut */
  assert(flag==1 || flag==2);
  double ptSum=0;
  
  //.... process BTOW hits
  for(int i=0;i< mxBtow;i++) {
    float ene=wEve.bemc.eneTile[kBTow][i];
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
    if(primP.Perp()>par_countTowEt) nTow++;
    ptSum+=primP.Perp();
  }
  
  return ptSum;  
}


//________________________________________________
//________________________________________________
float
St2009WMaker::sumEtowCone(float zVert, TVector3 refAxis, int flag,int &nTow){
  /* flag=1 : only delta phi cut;  flag=2 use 2D cut */
  assert(flag==1 || flag==2);
  
  float ptsum=0;
  
  //....loop over all phi bins
  for(int iphi=0; iphi<mxEtowPhiBin; iphi++){
    for(int ieta=0; ieta<mxEtowEta; ieta++){//sum all eta rings
      float ene=wEve.etow.ene[iphi][ieta];
      if(ene<=0) continue; //skip towers with no energy
      TVector3 primP=positionEtow[iphi][ieta]-TVector3(0,0,zVert);
      primP.SetMag(ene); // it is 3D momentum in the event ref frame
      if(flag==1) {
        float deltaPhi=refAxis.DeltaPhi(primP);
        if(fabs(deltaPhi)> par_awayDeltaPhi) continue;
      }
      if(flag==2) {
        float deltaR=refAxis.DeltaR(primP);
        if(deltaR> par_nearDeltaR) continue;
      }
      if(primP.Perp()>par_countTowEt) nTow++;
      ptsum+=primP.Perp();
    }
  }
  
  return ptsum;
}

// $Log: St2009W_algo.cxx,v $
// Revision 1.25  2011/09/14 14:23:20  stevens4
// update used for cross section PRD paper
//
// Revision 1.24  2010/04/27 16:53:45  stevens4
// add code to remove events tagged as Zs from W candidates
//
// Revision 1.23  2010/04/16 14:35:32  balewski
// fix borken header
//
// Revision 1.22  2010/03/23 01:31:40  seelej
// Fix to the filling of the histograms for the background systematic.
//
// Revision 1.21  2010/03/22 01:45:58  seelej
// *** empty log message ***
//
// Revision 1.20  2010/03/22 01:33:13  seelej
// Additional change.
//
// Revision 1.19  2010/03/22 00:55:45  seelej
// Change to use the signed pT balance in background subtraction instead of the unsigned pT balance.
//
// Revision 1.18  2010/03/18 18:46:40  balewski
// simplified sPtBalance calculation
//
// Revision 1.17  2010/03/18 16:52:17  balewski
// corrected sPtBalance for no-endcap
//
// Revision 1.16  2010/03/18 15:34:43  seelej
// Changed the definition of sPtBalance - Joe
//
// Revision 1.15  2010/03/14 22:50:31  balewski
// *** empty log message ***
//
// Revision 1.14  2010/03/12 21:08:11  balewski
// simplify logic for filling histos for noE background
//
// Revision 1.13  2010/02/26 21:40:00  seelej
// Joe : Fix to code. Forgot to remove an older piece of code when doing a previous update.
//
// Revision 1.12  2010/02/22 15:49:34  seelej
// Joe : Changes to code for inclusion of background subtraction and systematic studies
//
// Revision 1.11  2010/01/28 03:42:55  balewski
// cleanup
//
// Revision 1.10  2010/01/27 22:12:24  balewski
// spin code matched to x-section code
//
// Revision 1.9  2010/01/26 18:48:11  stevens4
// include |eta|<1 cut for all W yields
//
// Revision 1.8  2010/01/23 02:35:38  stevens4
// add ability to scale jet et and use real btow peds for rcf mc
//
// Revision 1.7  2010/01/10 03:01:37  balewski
// cleanup & nicer histos
//
// Revision 1.6  2010/01/10 01:45:10  stevens4
// fix plots w/o EEMC in veto
//
// Revision 1.5  2010/01/09 00:07:16  stevens4
// add jet finder
//
// Revision 1.4  2010/01/06 19:16:47  stevens4
// track cuts now on primary component, cleanup
//
// Revision 1.3  2010/01/06 14:11:13  balewski
// one Z-plot added
//
// Revision 1.2  2010/01/06 04:22:15  balewski
// added Q/PT plot for Zs, more cleanup
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
// 
