// $Id: St2011W_Ealgo.cxx,v 1.23 2013/09/13 19:33:13 stevens4 Exp $
//
//*-- Author : Jan Balewski, MIT
//*-- Author for Endcap: Justin Stevens, IUCF

#include "TF1.h"

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "WeventDisplay.h"
#include "StSpinPool/StJets/StJet.h"
#include "StSpinPool/StJets/TowerToJetIndex.h"

#include "St2011WMaker.h"

//________________________________________________
//________________________________________________
void
St2011WMaker::findEndcap_W_boson(){

  if(!wEve->l2EbitET) return;

  //printf("========= findEndcap_W_boson() \n");
  int nNoNear=0,nSmdRatio=0,nNoAway=0,nGoldW=0,nGoldWp=0,nGoldWn=0;
  //remove events tagged as Zs
  if(wEve->zTag) return;

  // search for  Ws ............ 
  for(uint iv=0;iv<wEve->vertex.size();iv++) {
    WeveVertex &V=wEve->vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.pointTower.id>=0) continue; //skip barrel towers
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error

      //signal plots w/o EEMC in awayside veto
      if(T.cluster.ET/T.nearTotET_noEEMC>parE_nearTotEtFrac){	
	if(T.sPtBalance_noEEMC>parE_ptBalance ) {//only signed ptBalance cut 
	  hE[140]->Fill(T.cluster.ET);
	  if (T.prMuTrack->charge() < 0) {
	    hE[184+3]->Fill(T.cluster.ET);
	  } else if (T.prMuTrack->charge() > 0) {
	    hE[184+4]->Fill(T.cluster.ET);
	  }
	}
      }

      // track matched to cluster plots
      StThreeVectorF ri=T.glMuTrack->firstPoint();
      StThreeVectorF ro=T.glMuTrack->lastPoint();
      int sec = WtpcFilter::getTpcSec(ro.phi(),ro.pseudoRapidity());
      if((sec < 5 || sec > 7) && sec!=21) { //skip sectors with dead padrows for this
	hE[63]->Fill(T.glMuTrack->nHitsFit());
	hE[64]->Fill(1.*T.glMuTrack->nHitsFit()/T.glMuTrack->nHitsPoss());
	hE[65]->Fill(ri.perp());
	hE[65]->Fill(ro.perp());
      }

      if(T.cluster.ET /T.nearTotET< parE_nearTotEtFrac) continue; // too large nearET

      hE[20]->Fill("noNear",1.);
      nNoNear++;
      hE[112]->Fill( T.cluster.ET); // for Joe
      hE[50]->Fill(T.awayTpcPT);
      hE[51]->Fill(T.awayBtowET);
      hE[54]->Fill(T.awayTotET);
      hE[52]->Fill(T.cluster.ET,T.awayTotET);
      hE[53]->Fill(T.cluster.ET,T.awayEmcET);
      hE[55]->Fill(T.awayEtowET);
      hE[60]->Fill(T.cluster.ET,T.awayTpcPT);

      hE[132]->Fill(T.cluster.ET,T.ptBalance.Perp());
      hE[133]->Fill(T.awayTotET,T.ptBalance.Perp());
      hE[134]->Fill(T.cluster.ET,T.sPtBalance);
      hE[135]->Fill(T.awayTotET,T.sPtBalance);
      if(T.cluster.ET > parE_highET) hE[239]->Fill(T.awayTotET,T.sPtBalance);

      //alternate sPtBalance
      hE[141]->Fill(T.sPtBalance,T.sPtBalance-T.sPtBalance2);

      //plots for backg sub yield
      if(T.sPtBalance>parE_ptBalance ) {
        hE[136]->Fill(T.cluster.ET);//signal
        hE[62]->Fill(T.pointTower.iEta ,T.cluster.energy);
        if (T.prMuTrack->charge() < 0) {
          hE[184+1]->Fill(T.cluster.ET);
        } else if (T.prMuTrack->charge() > 0) {
          hE[184+2]->Fill(T.cluster.ET);
        }
      } else {
        hE[137]->Fill(T.cluster.ET);//background
        if (T.prMuTrack->charge() < 0) {
          hE[184+5]->Fill(T.cluster.ET);
        } else if (T.prMuTrack->charge() > 0) {
          hE[184+6]->Fill(T.cluster.ET);
        }
      }
      
      //some ESMD QA plots
      if(T.sPtBalance2>parE_ptBalance){
        hE[214]->Fill(T.cluster.ET,T.esmdE[0]+T.esmdE[1]);
        hE[215]->Fill(T.cluster.ET,T.esmdNhit[0]+T.esmdNhit[1]);
        hE[220]->Fill(T.cluster.ET,(T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]));
        hE[223]->Fill(T.cluster.ET,T.enePre1+T.enePre2);
        hE[224]->Fill(T.cluster.ET,T.enePost);
        hE[227]->Fill(T.cluster.ET,T.esmdPeakSumE[0]+T.esmdPeakSumE[1]);
        hE[228]->Fill(T.cluster.ET,T.esmdMaxADC);
	
        if(T.cluster.ET>parE_highET) { //most W like
          hE[211]->Fill(T.esmdNhit[0],T.esmdNhit[1]);
          hE[212]->Fill(T.esmdE[0],T.esmdE[1]);
          hE[213]->Fill(T.esmdNhit[0]+T.esmdNhit[1],T.esmdE[0]+T.esmdE[1]);
          hE[217]->Fill(T.pointTower.R.X()-T.esmdXPcentroid.X(),T.pointTower.R.Y()-T.esmdXPcentroid.Y());
          hE[218]->Fill(T.pointTower.R.Eta()-T.esmdXPcentroid.Eta(),T.pointTower.R.Phi()-T.esmdXPcentroid.Phi());
          hE[219]->Fill(T.esmdPeakSumE[0]/T.esmdE[0],T.esmdPeakSumE[1]/T.esmdE[1]);
	  
          hE[221]->Fill(T.enePre1,T.enePre2);
          hE[222]->Fill(T.enePre1+T.enePre2,T.enePost);
          hE[225]->Fill(T.enePre1+T.enePre2,T.esmdE[0]+T.esmdE[1]);
          hE[226]->Fill(T.enePost,T.esmdE[0]+T.esmdE[1]);
	  hE[256]->Fill(T.pointTower.R.Phi(), T.esmdPeakOffset[0]);
	  hE[257]->Fill(T.pointTower.R.Phi(), T.esmdPeakOffset[1]);
        }
      }
      else { //mostly QCD
        if(T.cluster.ET>parE_highET) hE[235]->Fill(T.esmdPeakSumE[0]/T.esmdE[0],T.esmdPeakSumE[1]/T.esmdE[1]);
        hE[236]->Fill(T.cluster.ET,(T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]));
      }
      
      //define charge separation
      float q2pt_g = T.glMuTrack->charge()/T.glMuTrack->pt();
      float q2pt_p = T.prMuTrack->charge()/T.prMuTrack->pt();
      float hypCorr_g = q2pt_g*(T.cluster.ET);
      float hypCorr_p = q2pt_p*(T.cluster.ET);

      //correlate ratio with sPtBal for goldWs (used for background)
      if( fabs(hypCorr_p) > parE_QET2PTlow && fabs(hypCorr_p) < parE_QET2PThigh) { //remove ambiguous charges BG treatment
	if(T.cluster.ET > parE_highET && T.cluster.ET < 50.){
	  hE[237]->Fill(T.sPtBalance,(T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]));
	  hE[238]->Fill(T.sPtBalance2,(T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]));
	  if(T.prMuTrack->charge()>0) 
	    hE[250]->Fill(T.sPtBalance2,(T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]));
	  else
	    hE[251]->Fill(T.sPtBalance2,(T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]));
	}
	// try lower threshold to find background
	if(T.cluster.ET > 20.&& T.cluster.ET < 50.){
	  hE[252]->Fill(T.sPtBalance,(T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]));
	  hE[253]->Fill(T.sPtBalance2,(T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]));
	  if(T.prMuTrack->charge()>0) 
	    hE[254]->Fill(T.sPtBalance2,(T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]));
	  else
	    hE[255]->Fill(T.sPtBalance2,(T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]));
	}
      }

      //fail ratio cut at 0.5 (mostly BG)
      if((T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]) < 0.5){
	hE[232]->Fill(T.cluster.ET,T.sPtBalance);
        hE[233]->Fill(T.cluster.ET,T.sPtBalance2);
      }

      //event display
      if(T.sPtBalance2>parE_ptBalance && (T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]) > parE_smdRatio){/***************************/
        printf("\n WWWWWWWWWWWWWWWWWWWWW  Endcap \n");
        wDisaply->exportEvent( "WE", V, T, iv);
        wEve->print();
	//	assert(1==2);
      }/***************************/

      //cut on ESMD ratio
      if((T.esmdPeakSumE[0]+T.esmdPeakSumE[1])/(T.esmdE[0]+T.esmdE[1]) < parE_smdRatio)
        continue;
      
      hE[20]->Fill("smdRatio",1.0);
      nSmdRatio++;

      hE[113]->Fill( T.cluster.ET);//for Joe
      hE[230]->Fill(T.cluster.ET,T.sPtBalance);
      hE[231]->Fill(T.cluster.ET,T.sPtBalance2);
      
      //put final W cut here
      if(T.sPtBalance2<parE_ptBalance)  continue;
      //::::::::::::::::::::::::::::::::::::::::::::::::
      //:::::accepted W events for x-section :::::::::::
      //::::::::::::::::::::::::::::::::::::::::::::::::

      hE[20]->Fill("noAway",1.0);
      nNoAway++;
      hE[114]->Fill( T.cluster.ET);//for Joe

      hE[90]->Fill( T.cluster.ET);
      hE[92]->Fill( T.cluster.ET,T.glMuTrack->dEdx()*1e6);
      //hE[93]->Fill( T.cluster.ET,T.glMuTrack->dca().mag());
      int k=0; if(T.prMuTrack->charge()<0) k=1;
      hE[94+k]->Fill( T.cluster.ET,T.glMuTrack->dcaD());
      // h95 used above

      // do charge sign plot
      float ET=T.cluster.ET;
      const StMuTrack *glTr=T.glMuTrack; assert(glTr);
      const StMuTrack *prTr=T.prMuTrack; assert(prTr);
      float g_chrg=glTr->charge();
      float p_chrg=prTr->charge();
      hE[200]->Fill(ET,g_chrg/glTr->pt());
      hE[201]->Fill(ET,p_chrg/prTr->pt());
      hE[202]->Fill(T.cluster.ET,hypCorr_g);
      hE[203]->Fill(T.cluster.ET,hypCorr_p);

      //charge sign flip with vertex refit
      int g_ipn=0, p_ipn=0; // plus
      if( g_chrg<0 ) g_ipn=1;// minus
      if( p_chrg<0 ) p_ipn=1;// minus
      hE[240+g_ipn]->Fill(ET);
      hE[242+p_ipn]->Fill(ET);
      if(g_chrg* p_chrg <-0.5) hE[244+p_ipn]->Fill(ET); // charge flip

      if(T.cluster.ET<parE_highET) continue;  // very likely Ws
      hE[91]->Fill(T.cluster.position.PseudoRapidity(),T.cluster.position.Phi());
      hE[96]->Fill(V.id);
      hE[97]->Fill(V.funnyRank);
      hE[98]->Fill(V.z);
      hE[99]->Fill( T.prMuTrack->eta());
      hE[100]->Fill(T.pointTower.R.X(),T.pointTower.R.Y());
      hE[190+k]->Fill(T.prMuTrack->eta(),T.cluster.ET);
      hE[101]->Fill(T.cluster.ET/T.cl4x4.ET,T.sPtBalance);
      hE[102]->Fill(T.cluster.ET/T.nearTotET,T.sPtBalance);
      hE[103]->Fill(T.cluster.ET/T.nearEmcET,T.sPtBalance);
      hE[104]->Fill(T.cluster.ET/T.nearEtowET,T.sPtBalance);
      hE[105]->Fill(T.glMuTrack->dEdx()*1e6,T.sPtBalance);
      
      // plots to study charge separation
      hE[106]->Fill(T.prMuTrack->chi2(),hypCorr_p);
      hE[107]->Fill(T.glMuTrack->chi2(),hypCorr_p);
      hE[108]->Fill(1.*T.glMuTrack->nHitsFit()/T.glMuTrack->nHitsPoss(),hypCorr_p);
      hE[109]->Fill(T.glMuTrack->nHitsFit(),hypCorr_p);
      
      hE[120]->Fill(T.prMuTrack->chi2()-T.glMuTrack->chi2(),hypCorr_p);
      hE[121]->Fill(T.glMuTrack->dcaD(),hypCorr_p);
      hE[122]->Fill(T.esmdPeakOffset[0],hypCorr_p);
      hE[123]->Fill(T.esmdPeakOffset[1],hypCorr_p);
      hE[124]->Fill(T.esmdPeakOffset[1],T.esmdPeakOffset[0]);
      hE[125]->Fill(ri.perp(),hypCorr_p);
      hE[126]->Fill(ro.perp(),hypCorr_p);
      hE[127]->Fill(T.glMuTrack->dEdx()*1e6,hypCorr_p);
      hE[128]->Fill(T.glMuTrack->lengthMeasured(),hypCorr_p);
      hE[129]->Fill(ro.perp()-ri.perp(),hypCorr_p);
      float phiScale = 180./TMath::Pi();
      float deltaPhiESMD = (T.pointTower.R.Phi()-T.esmdXPcentroid.Phi())*phiScale;
      float deltaPhiTrack = (T.pointTower.R.Phi()-T.pointTower.Rglob.Phi())*phiScale;
      float deltaR = ro.perp()-ri.perp();
      float chi2 = T.prMuTrack->chi2();
      hE[130]->Fill(deltaPhiESMD,hypCorr_p);
      hE[131]->Fill(deltaPhiTrack,hypCorr_p);
      
      if(hypCorr_p > parE_QET2PTlow && hypCorr_p < parE_QET2PThigh) {
	hE[150]->Fill(T.glMuTrack->dcaD(),ro.perp());
	hE[152]->Fill(deltaPhiESMD,ro.perp());
	hE[154]->Fill(deltaPhiESMD,ro.perp()-ri.perp());
	hE[156]->Fill(deltaPhiTrack,ro.perp()-ri.perp());
	hE[158]->Fill(deltaPhiTrack,T.prMuTrack->chi2());
	hE[160]->Fill(deltaPhiESMD,T.prMuTrack->chi2());
	hE[162]->Fill(ro.perp()-ri.perp(),T.prMuTrack->chi2());
      }
      else if(hypCorr_p < -1.*parE_QET2PTlow && hypCorr_p > -1.*parE_QET2PThigh) {
	hE[151]->Fill(T.glMuTrack->dcaD(),ro.perp());
	hE[153]->Fill(deltaPhiESMD,ro.perp());
	hE[155]->Fill(deltaPhiESMD,ro.perp()-ri.perp());
	hE[157]->Fill(deltaPhiTrack,ro.perp()-ri.perp());
	hE[159]->Fill(deltaPhiTrack,T.prMuTrack->chi2());
	hE[161]->Fill(deltaPhiESMD,T.prMuTrack->chi2());
	hE[163]->Fill(ro.perp()-ri.perp(),T.prMuTrack->chi2());
      }

      // harder "or" cuts
      float delRcut = 45.; //50
      float RoutCut = 110.; //115
      float delPhiCut = 0.25; //0.2
      float chi2cut = 3.0; //2.5
      if( (chi2 < chi2cut && fabs(deltaPhiTrack) < delPhiCut) || (chi2 < chi2cut && deltaR > delRcut) || (fabs(deltaPhiTrack) < delPhiCut && deltaR > delRcut) )
	hE[170]->Fill(T.glMuTrack->nHitsFit(),hypCorr_p);
      if( (chi2 < chi2cut && fabs(deltaPhiTrack) < delPhiCut) || (chi2 < chi2cut && ro.perp() > RoutCut) || (fabs(deltaPhiTrack) < delPhiCut && ro.perp() > RoutCut) )
	hE[171]->Fill(T.glMuTrack->nHitsFit(),hypCorr_p);
      
      // softer "and" cuts
      if(chi2 < 4.0 && fabs(deltaPhiTrack) < 0.4 && ro.perp() > 90.) //3.5 0.35 90 (old cuts)
	hE[172]->Fill(T.glMuTrack->nHitsFit(),hypCorr_p);
      if(chi2 < 3.5 && fabs(deltaPhiTrack) < 0.4 && ro.perp() > 90.) 
	hE[173]->Fill(T.glMuTrack->nHitsFit(),hypCorr_p);
      if(chi2 < 4.0 && fabs(deltaPhiTrack) < 0.4)
	hE[174]->Fill(T.glMuTrack->nHitsFit(),hypCorr_p);
      if(chi2 < 4.0 && fabs(deltaPhiTrack) < 0.35)
	hE[175]->Fill(T.glMuTrack->nHitsFit(),hypCorr_p);
      if(chi2 < 3.5 && fabs(deltaPhiTrack) < 0.4)
	hE[176]->Fill(T.glMuTrack->nHitsFit(),hypCorr_p);
       
      hE[20]->Fill("goldW",1.);
      nGoldW++;
      if(T.prMuTrack->charge()>0) nGoldWp++;
      else if(T.prMuTrack->charge()<0) nGoldWn++;

    }// loop over tracks
  }// loop over vertices
  if(nNoNear>0) hE[0]->Fill("noNear",1.);
  if(nSmdRatio>0) hE[0]->Fill("smdRatio",1.);
  if(nNoAway>0) hE[0]->Fill("noAway",1.);
  if(nGoldW>0)  hE[0]->Fill("goldW",1.);
  if(nGoldWp>0) hE[0]->Fill("goldW+",1.);
  if(nGoldWn>0) hE[0]->Fill("goldW-",1.);

}

//________________________________________________
//________________________________________________
void
St2011WMaker::analyzeESMD(){
  if(!wEve->l2EbitET) return;

  //printf("========= analyzeESMD \n");
  for(uint iv=0;iv<wEve->vertex.size();iv++) {
    WeveVertex &V=wEve->vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.pointTower.id>=0) continue; //skip barrel towers
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
          
      //id of strips pointed by prim and glob tracks in each plane
      int hitStrip[2]={-1,-1}; int hitStripGlob[2]={-1,-1};

      int isec=T.pointTower.iPhi/mxEtowSub;
      int layer[2]={((isec+2)%3) +1, (isec%3) +1};
   
      for(int iuv=0; iuv<2; iuv++){ //loop over planes

	//.... extrapolate track to the disk perpendicular to the z-axis
	const StPhysicalHelixD trkHlx=T.prMuTrack->outerHelix();    
	// to account for the z-depth of each plane, sector dependent, (cm), Z=smd depth
	StThreeVectorD diskPosition=StThreeVectorD(0,0,geomE->getZSMD() + (layer[iuv]-2)* 1.25);
	StThreeVectorD diskNormal=StThreeVectorD(0,0,1);
	printf(" ESMD sec=%d iuv=%d  layer=%d, smdZ=%.1f\n", isec+1, iuv,layer[iuv],diskPosition.z() );
	//path length at intersection with plane
	double path = trkHlx.pathLength(diskPosition,diskNormal);	
	StThreeVectorD r = trkHlx.at(path);
	TVector3 rCross(r.x(),r.y(),r.z());

        Float_t dca; //primary extrapolation to smd plane
	const StructEEmcStrip *stripPtr = geoSmd->getDca2Strip(iuv,rCross,&dca); // find pointed strip
        if(!stripPtr) {cout<<"No Strip found"<<endl; continue;}
        if(fabs(dca)>0.51 /*cm*/) {cout<<"Esmd DCA to big ="<<dca<<endl; continue;}
	
        Float_t dcaGlob; //global extrapolation to smd plane
	const StPhysicalHelixD trkHlxGlob=T.glMuTrack->outerHelix();
	double pathGlob = trkHlxGlob.pathLength(diskPosition,diskNormal);
	StThreeVectorD rGlob = trkHlxGlob.at(pathGlob);
	TVector3 rCrossGlob(rGlob.x(),rGlob.y(),rGlob.z());	
	const StructEEmcStrip *stripPtrGlob = geoSmd->getDca2Strip(iuv,rCrossGlob,&dcaGlob); // find pointed strip

        int stripId=stripPtr->stripStructId.stripId;
        int sectorId=stripPtr->stripStructId.sectorId;
        T.hitSector=sectorId;
        T.esmdGlobStrip[iuv]=stripPtrGlob->stripStructId.stripId-stripId;
        T.esmdDca[iuv]=dca; T.esmdDcaGlob[iuv]=dcaGlob;
        hitStrip[iuv]=stripId; hitStripGlob[iuv]=stripPtrGlob->stripStructId.stripId;
	
	// set integration range for smd energy
        int str1=stripId - parE_nSmdStrip; if(str1<1) str1=1;
        int str2=stripId + parE_nSmdStrip; if(str2>288) str2=288;
        for(int istrip=str1; istrip<=str2; istrip++){
          float ene = wEve->esmd.ene[sectorId-1][iuv][istrip-1]*1e3; // in MeV now
	  int adc = wEve->esmd.adc[sectorId-1][iuv][istrip-1];
	  T.esmdShower[iuv][istrip-stripId+parE_nSmdStrip]=ene;
	  if(adc > T.esmdMaxADC){ T.esmdMaxADC=adc; }
          if(ene > 0){
            T.esmdE[iuv]+=ene; //total energy
            T.esmdNhit[iuv]++;
	    if(abs(istrip-stripId)<4){ //7 central sum
              //cout<<istrip<<" "<<stripId<<" "<<ene<<endl;
              T.esmdEsum7[iuv]+=ene;
            }
          }
        }// end loop over strips

	//  finding smd-peak center correction
	float bestSum=-1; 
	int bestOff=9999;
	int delOff=parE_esmdWL -parE_esmdGL;
	for(int off1=-delOff; off1<=delOff; off1++) {
	  float sum=0;
	  for(int off2=-parE_esmdGL; off2<=parE_esmdGL; off2++)
	    sum+=T.esmdShower[iuv][parE_nSmdStrip+off1+off2];
	  //printf("off1=%d sum=%.1f  bestSum=%.1f bestOff=%d\n",off1,sum,bestSum,bestOff);
	  if(bestSum>sum) continue;
	  bestSum=sum;
	  bestOff=off1;
	}
	printf("do slide iuv=%d  sum7=%.1f  bestSum=%.1f bestOff=%d\n",iuv, T.esmdEsum7[iuv], bestSum,bestOff);
	T.esmdPeakSumE[iuv]=bestSum;	
	T.esmdPeakOffset[iuv]=bestOff;

      } //end plane loop

      //get shower x-point from hitStrip + centroid of fit
      T.esmdXPcentroid = geoSmd->getIntersection(T.hitSector-1,hitStrip[0]-1+T.esmdPeakOffset[0],hitStrip[1]-1+T.esmdPeakOffset[1]);//janCheck
	
    } //end track loop
  } //end vertex loop
}


//________________________________________________
//________________________________________________
void
St2011WMaker::analyzeEPRS(){
  if(!wEve->l2EbitET) return;

  //printf("========= analyzeEPRS \n");
  for(uint iv=0;iv<wEve->vertex.size();iv++) {
    WeveVertex &V=wEve->vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.pointTower.id>=0) continue; //skip barrel towers
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error

      //do some clustering of EPRS deposits and plot histos
      T.enePre1 = wEve->eprs.ene[T.cluster.iPhi][T.cluster.iEta][0];
      T.enePre2 = wEve->eprs.ene[T.cluster.iPhi][T.cluster.iEta][1];
      T.enePost = wEve->eprs.ene[T.cluster.iPhi][T.cluster.iEta][2];

    }
  }

}


//________________________________________________
//________________________________________________
float
St2011WMaker::sumEtowCone(float zVert, TVector3 refAxis, int flag){
  /* flag=1 : only delta phi cut;  flag=2 use 2D cut */
  assert(flag==1 || flag==2);
  float ptsum=0;

  //....loop over all phi bins
  for(int iphi=0; iphi<mxEtowPhiBin; iphi++){
    for(int ieta=0; ieta<mxEtowEta; ieta++){//sum all eta rings
      float ene=wEve->etow.ene[iphi][ieta];
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
      ptsum+=primP.Perp();
    }
  }
  
  return ptsum;
}


// ************* Endcap Code ************ //
// ************************************** //

//________________________________________________
//________________________________________________
int
St2011WMaker::extendTrack2Endcap(){// return # of extended tracks
  //printf("******* extendTracksEndcap() nVert=%d\n",wEve.vertex.size());
  if(!wEve->l2EbitET) return 0; //fire endcap trigger

  double parE_zSMD=geomE->getZSMD(); // (cm), smd depth
  int nTrE=0;
  for(uint iv=0;iv<wEve->vertex.size();iv++) {
    WeveVertex &V=wEve->vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.prMuTrack->eta()<parE_trackEtaMin) 
	continue; // to avoid extrapolation nonsense

      //do eta sorting at track level (tree analysis)
      if(T.primP.Eta() < parE_leptonEtaLow || T.primP.Eta() > parE_leptonEtaHigh) continue;
      
      //.... extrapolate track to the disk perpendicular to the z-axis
      const StPhysicalHelixD trkHlx=T.prMuTrack->outerHelix(); 
      StThreeVectorD diskPosition=StThreeVectorD(0,0,parE_zSMD);
      StThreeVectorD diskNormal=StThreeVectorD(0,0,1);

      //path length at intersection with plane
      double path = trkHlx.pathLength(diskPosition,diskNormal);

      StThreeVectorD r = trkHlx.at(path);
      float periodL=trkHlx.period();

      if(periodL<2*path) {
        printf(" Warn, long path fac=%.1f ",path/periodL);
        printf(" punchEEMC1 x,y,z=%.1f, %.1f, %.1f path=%.1f period=%.1f\n",r.x(),r.y(),r.z(),path,periodL);
      }

      //printf("hitR xyz=%f %f %f, detEta=%f\n",r.x(),r.y(),r.z(),eta);
      hE[69]->Fill(r.x(), r.y());

      int isec,isubSec,ietaBin;
      Float_t epsPhi, epsEta;
      TVector3 rCross(r.x(),r.y(),r.z());
      bool inEtow=geomE->getTower(rCross,isec,isubSec,ietaBin,epsPhi,epsEta);
      if(!inEtow) continue;
      hE[20]->Fill("@E",1.);
      //printf("trk points EEMC tower isec=%d isub=%d ieta=%d epsPhi=%f epsEta=%f  trkPT=%f\n", isec,isubSec,ietaBin,epsPhi,epsEta,T.prMuTrack->pt());

      nTrE++;
      T.pointTower.id=-999; //set negative for endcap towers
      T.pointTower.R=rCross;
      T.pointTower.iEta=ietaBin;
      T.pointTower.iPhi=isec*mxEtowSub+isubSec;

      //find global track extrapolation (for ESMD analysis)
      const StPhysicalHelixD trkHlxGlob=T.glMuTrack->outerHelix();
      double pathGlob = trkHlxGlob.pathLength(diskPosition,diskNormal);

      StThreeVectorD rGlob = trkHlxGlob.at(pathGlob);
      float periodLGlob=trkHlxGlob.period();

      if(periodLGlob<2*pathGlob) {
        printf(" Warn, long path Global fac=%.1f ",pathGlob/periodLGlob);
        printf(" punchEEMC1 x,y,z=%.1f, %.1f, %.1f path=%.1f period=%.1f\n",r.x(),r.y(),r.z(),pathGlob,periodLGlob);
      }
      TVector3 rCrossGlob(rGlob.x(),rGlob.y(),rGlob.z());
      T.pointTower.Rglob=rCrossGlob;

    }
  }// end of loop over vertices

  if(nTrE<=0) return -1; 
  hE[0]->Fill("TrE",1.0);
  return 0;

}


//________________________________________________
//________________________________________________
int
St2011WMaker::matchTrack2EtowCluster(){
  //printf("******* matchEtowCluster() nVert=%d\n",wEve.vertex.size());
  
  if(!wEve->l2EbitET) return 0;

  int nTr=0;
  for(uint iv=0;iv<wEve->vertex.size();iv++) {
    WeveVertex &V=wEve->vertex[iv];
    float zVert=V.z;
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.pointTower.id>=0) continue; //skip barrel towers
            
      float trackPT=T.prMuTrack->momentum().perp();
      T.cluster=maxEtow2x2(T.pointTower.iEta,T.pointTower.iPhi,zVert);
      hE[110]->Fill( T.cluster.ET);
      hE[33]->Fill(T.cluster.ET);
      hE[34]->Fill(T.cluster.adcSum,trackPT);

      // ........compute surrounding cluster energy
      int iEta=T.cluster.iEta;
      int iPhi=T.cluster.iPhi;
      T.cl4x4=sumEtowPatch(iEta-1,iPhi-1,4,4,zVert);

      if (T.cluster.ET<parE_clustET) continue; // too low energy
      hE[20]->Fill("CL",1.);
      hE[37]->Fill(T.cl4x4.ET);
      hE[38]->Fill(T.cluster.energy, T.cl4x4.energy-T.cluster.energy);

      float frac24=T.cluster.ET/(T.cl4x4.ET);
      hE[39]->Fill(frac24);
      if(frac24<parE_clustFrac24) continue;
      hE[20]->Fill("fr24",1.);

      //set logE weighted cluster position vector at SMD z depth
      float newMag=geomE->getZSMD()/TMath::Cos(T.cluster.position.Theta());
      T.cluster.position.SetMag(newMag);

      //.. spacial separation (track - cluster) only use 2D X-Y distance for endcap (ie. D.Perp())
      TVector3 D=T.pointTower.R-T.cluster.position;
      hE[43]->Fill(T.cluster.energy,D.Perp());
      float delPhi=T.pointTower.R.DeltaPhi(T.cluster.position);
      float Rxy=T.cluster.position.Perp();

      hE[44]->Fill( T.cluster.position.Phi(),Rxy*delPhi);
      hE[45]->Fill( T.cluster.energy,Rxy*delPhi);// wrong?
      hE[46]->Fill( D.Perp());
      
      if(D.Perp()>par_delR3D) continue;
      T.isMatch2Cl=true; // cluster is matched to TPC track
      hE[20]->Fill("#Delta R",1.);
      hE[111]->Fill( T.cluster.ET);
      
      nTr++;
    }// end of one vertex
  }// end of vertex loop

  if(nTr<=0) return -1; 
  hE[0]->Fill("Tr2Cl",1.0);
  return 0;

}


//________________________________________________
//________________________________________________ 
WeveCluster
St2011WMaker::maxEtow2x1(int iEta, int iPhi, float zVert){
  //printf("   maxEtow2x1  seed iEta=%d iPhi=%d \n",iEta, iPhi);

  WeveCluster maxCL;
  // just 4 cases of 2x1 clusters
  float maxET=0;
  int I0=iEta-1;
  int J0=iPhi-1;
  for(int I=I0;I<=I0+1;I++){ // try along eta dir
    WeveCluster CL=sumEtowPatch(I,iPhi,2,1,zVert);
    if(maxET>CL.ET) continue;
    maxET=CL.ET;
    maxCL=CL;
    //printf(" maxEtow2x1 A  newMaxETSum=%.1f iEta=%d iPhi=%d \n",maxET, I,iPhi);
  }

  for(int J=J0;J<=J0+1;J++) { // try along phi dir
    WeveCluster CL=sumEtowPatch(iEta,J,1,2,zVert);
    if(maxET>CL.ET) continue;
    maxET=CL.ET;
    maxCL=CL;
    //printf(" maxEtow2x1 B  newMaxETSum=%.1f iEta=%d iPhi=%d \n",maxET,iEta,J);
  }
  //printf(" final inpEve=%d SumET2x2=%.1f \n",nInpEve,maxET);
  return maxCL;
}


//________________________________________________
//________________________________________________ 
WeveCluster
St2011WMaker::maxEtow2x2(int iEta, int iPhi, float zVert){
  //printf("   maxEtow2x1  seed iEta=%d iPhi=%d \n",iEta, iPhi);
  const int L=2; // size of the summed square

  WeveCluster maxCL;
  // just 4 cases of 2x1 clusters
  float maxET=0;
  int I0=iEta-1;
  int J0=iPhi-1;
  for(int I=I0;I<=I0+1;I++){
    for(int J=J0;J<=J0+1;J++) {
      WeveCluster CL=sumEtowPatch(I,J,L,L,zVert);
      if(maxET>CL.ET) continue;
      maxET=CL.ET;
      maxCL=CL;
      //printf(" maxEtow2x2 A  newMaxETSum=%.1f iEta=%d iPhi=%d \n",maxET, I,iPhi);
    }
  }// 4 combinations done

  //printf(" final inpEve=%d SumET2x2=%.1f \n",nInpEve,maxET);
  return maxCL;
}


//________________________________________________
//________________________________________________
WeveCluster
St2011WMaker::sumEtowPatch(int iEta, int iPhi, int Leta,int  Lphi, float zVert){
  //printf("     eveID=%d etowPatch seed iEta=%d[+%d] iPhi=%d[+%d] zVert=%.0f \n",wEve.id,iEta,Leta, iPhi,Lphi,zVert);
  WeveCluster CL; // object is small, not to much overhead in creating it
  CL.iEta=iEta;
  CL.iPhi=iPhi;
  TVector3 R;
  double sumW=0;
  
  for(int i=iEta; i<iEta+Leta;i++){// trim in eta-direction
    if(i<0) continue;
    if(i>=mxEtowEta) continue;
    for(int j=iPhi;j<iPhi+Lphi;j++) {// wrap up in the phi-direction
      int jj=(j+mxEtowPhiBin)%mxEtowPhiBin;// keep it always positive
      //if(L<5) printf("n=%2d  i=%d jj=%d\n",CL.nTower,i,jj);

      float ene= wEve->etow.ene[jj][i];
      if(ene<=0) continue; // skip towers w/o energy
      float adc= wEve->etow.adc[jj][i];
      float delZ=positionEtow[jj][i].z()-zVert;
      float Rxy=positionEtow[jj][i].Perp();
      float e2et=Rxy/sqrt(Rxy*Rxy+delZ*delZ);
      float ET=ene*e2et;
      float logET=log10(ET+0.5);
      CL.nTower++;
      CL.energy+=ene;
      CL.ET+=ET;
      CL.adcSum+=adc;
      if(logET>0) {
        R+=logET*positionEtow[jj][i];
        sumW+=logET;
      }
    }
    //printf("      in etowPatch: iEta=%d  nTw=%d, ET=%.1f adcSum=%.1f\n",i,CL.nTower,CL.ET,CL.adcSum);
    if(sumW>0) {
      CL.position=1./sumW*R; // weighted cluster position
    } else {
      CL.position=TVector3(0,0,999);
    }
  }
  return CL;
}

// $Log: St2011W_Ealgo.cxx,v $
// Revision 1.23  2013/09/13 19:33:13  stevens4
// Updates to code for combined 2011+2012 result presented to spin PWG 9.12.13
//
// Revision 1.22  2012/10/01 19:48:20  stevens4
// add plots for Z result and move esmd cross point calculation outside plane loop
//
// Revision 1.21  2012/09/28 16:00:41  stevens4
// add Q*ET/PT requirement to WB histos used for background estimation to be consistent with spin sorting
//
// Revision 1.20  2012/09/26 14:20:59  stevens4
// use PtBal cos(phi) for WB and WE algos and use Q*ET/PT for barrel charge sign
//
// Revision 1.19  2012/09/26 01:10:51  stevens4
// apply R_ESMD cut using maximum of sliding window
//
// Revision 1.18  2012/09/21 21:14:04  balewski
// plane/sectord dependent Z-location for ESMD implemented in matching of TPC track to ESMD shower.
// I'm done
//
// Revision 1.17  2012/09/21 16:59:10  balewski
// added ESMD peak adjustement - partialy finished
//
// Revision 1.16  2012/09/18 22:30:18  stevens4
// change to new jet tree format with access to all rank>0 vertices
//
// Revision 1.15  2012/09/18 21:10:06  stevens4
// Include all rank>0 vertex again (new jet format coming next), and remove rank<0 endcap vertices.
//
// Revision 1.14  2012/09/17 22:05:50  stevens4
// exclude not-highest rank vertex until jet issue is resolved
//
// Revision 1.13  2012/09/17 03:29:29  stevens4
// Updates to Endcap algo and Q*ET/PT charge separation
//
// Revision 1.12  2012/08/31 20:10:51  stevens4
// switch to second EEMC background using both isolation and sPt-Bal (for mirror symmetry (also adjust eta binning)
//
// Revision 1.11  2012/08/28 14:28:27  stevens4
// add histos for barrel and endcap algos
//
// Revision 1.10  2012/08/21 21:28:22  stevens4
// Add spin sorting for endcap Ws
//
// Revision 1.9  2012/08/21 18:29:16  stevens4
// Updates to endcap W selection using ESMD strip ratio
//
// Revision 1.8  2012/08/07 21:06:38  stevens4
// update to tree analysis to produce independent histos in a TDirectory for each eta-bin
//
// Revision 1.7  2012/07/06 17:47:02  stevens4
// Updates for tree reader
//
// Revision 1.6  2012/06/25 20:53:19  stevens4
// algo and histo cleanup
//
// Revision 1.5  2012/06/18 18:28:00  stevens4
// Updates for Run 9+11+12 AL analysis
//
// Revision 1.4  2011/02/25 06:03:39  stevens4
// addes some histos and enabled running on MC
//
// Revision 1.3  2011/02/17 04:16:18  stevens4
// move sector dependent track QA cuts before track pt>10 cut and lower par_clustET and par_ptBalance thresholds to 14 GeV
//
// Revision 1.2  2011/02/14 01:36:17  stevens4
// *** empty log message ***
//
// Revision 1.1  2011/02/10 20:33:22  balewski
// start
//
