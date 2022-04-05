#ifdef __APPLE__
#include <sys/types.h>
#endif
#include <TH1F.h>
#include <TH2F.h>
#include <TObjArray.h>
#include <TGraph.h>
#include <TVector2.h>

#include "BarrelMipCalib.h"
#include "StJanBarrelDbMaker.h"
#include "JanBarrelEvent.h"

#include "StEmcUtil/geometry/StEmcGeom.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"


//________________________________________________
//________________________________________________
BarrelMipCalib::BarrelMipCalib( TObjArray *HList, StJanBarrelDbMaker* jdb,  StMuDstMaker *mumk){
    mJanDbMaker=jdb; assert(mJanDbMaker);
    muMaker=mumk;  assert(muMaker);
   
    assert(HList);
    memset(hA,0,sizeof(hA));


    hA[0]=new TH1F("mipStat","Mip calib statistics; case",30,-0.5,29.5);
    hA[1]=new TH1F("mipZver","primary (smalest) vertez Z ; vertx Z (cm)",200,-200,200);
    hA[2]=new TH1F("mipTrPt","primTr pT; pT (GeV/c)",100,0,20);
    hA[3]=new TH1F("mipTrEta","primTr Eta ; pseudorapidity",100,-2,2);
    hA[4]=new TH1F("mipTrNff","primTr nFit/nPos ; nFit/nPos",20,0,1.1);

    int nx=600, ny=720;
    hA[5]=new TH2F("mipTrRZ1","primTr punch BPRS  ; Z @ BPRS (cm); phi @ BPRS (rad)",nx,-300,300,ny,0,2*C_PI);
    hA[6]=new TH2F("mipTrRZ21","primTr punch BTOW L=21, input; Z @ BTOW layer=21 (cm); phi @ BPRS (rad)",nx,-300,300,ny,0,2*C_PI);
    hA[7]=new TH2F("mipTrRZf","primTr in BPRS fiducial ; Z @ BPRS (cm); phi @ BPRS (rad)",nx,-300,300,ny,0,2*C_PI);

    hA[8]=new TH2F("mipDeDx","primTr TPC deDx; track momentum (GeV); TPC dEdX (keV)", 100,0,5,100,0, 20);

    hA[9]=new TH1F("rankZver","highest rank  primary vertez Z ; vertx Z (cm)",200,-200,200);

    hA[10]=new TH1F("mipRxyTr","primTr Rxy of last point on the track; Rxy (cm)",220,0,220);
    int nb=200;
    float adc1=-50;
    float adc2=adc1+nb;
 
    //free 11
    hA[12]=new TH1F("mipTrTw","tower pointed by accepted prim track; BPRS softID",mxBtow,0.5, mxBtow+0.5);  
    // accepted events plots
    hA[13]=new TH1F("mipZverAc","primary vertez Z , accepted; vertx Z (cm)",200,-200,200);
    hA[14]=new TH1F("mipTrPtAc","primTr pT, accepted; pT (GeV/c)",100,0,20);
    hA[15]=new TH1F("mipTrPAc","primTr p, accepted; momentum (GeV)",100,0,20);

    // disable to de-activate swapSkan
    // hA[16]=new TH2F("swapScan","BTOW tile pointed by MIP vs. tiles with MIP-like ADC ; softID if ADC=MIP;  ID of track-tower",mxBtow,0.5, mxBtow+0.5,mxBtow,0.5, mxBtow+0.5);
    // 19 free
    hA[17]=new TH1F("mipTrZf","primTr in BPRS fiducial ; Z @ BPRS (cm)",1800,-300,300);
    hA[18]=new TH1F("mipTrRf","primTr in BPRS fiducial , z>0; phi @ BPRS (rad)",3600,0,2*C_PI);

    hA[20]=new TH1F("mipMaZ1","Z distance of track from both tower Z-edges;  delZ (cm)", 250,-5,20);
    hA[21]=new TH1F("mipMaP1","R*phi distance of track from tower edge;  delR*phi (cm)", 200,-10,10);

    hA[22]=new TH2F("mipBprsTr","ADC for BPRS pointed by TPC MIP track; BPRS softID; rawADC-ped",mxBtow,0.5,mxBtow+0.5, nb,adc1,adc2);
    hA[23]=new TH2F("mipBprsTrBt","ADC for BPRS pointed by TPC MIP track + BTOW=MIP; BPRS softID; rawADC-ped",mxBtow,0.5,mxBtow+0.5, nb,adc1,adc2);

    hA[24]=new TH2F("mipBtowTr","ADC for BTOW pointed by TPC track; BTOW softID; rawADC-ped",mxBtow,0.5,mxBtow+0.5, nb,adc1,adc2);
    hA[25]=new TH2F("mipBtowTrPr","ADC for BTOW pointed by TPC track+BPRS=MIP; BTOW softID; rawADC-ped",mxBtow,0.5,mxBtow+0.5, nb,adc1,adc2);



    for(int i=0;i<mxH;i++)
      if(hA[i]) HList->Add(hA[i]);
}




//________________________________________________
//________________________________________________
void BarrelMipCalib::search(  JanBarrelEvent &fullEve){

  // Access to muDst .......................
  StMuEvent* muEve = muMaker->muDst()->event();
  int nPrimV=muMaker->muDst()->numberOfPrimaryVertices();
  hA[0]->Fill(0);
  
#if 0
  StMuTriggerIdCollection &tic=muEve->triggerIdCollection();
  int trigID=96211;
  bool fired=tic.nominal().isTrigger(trigID);
#endif
  
  printf("\nmipCalib eventID %d nPrimV=%d  =============\n", muEve->eventId(),nPrimV);
  if(nPrimV<=0) return;
  hA[0]->Fill(1);

  
  int nPrimTr =0;
  
  // find vertex with smalest Z - to compensate for PPV error in FMS-production

  int iVert=-1;
  float zVert=99999;
  
  for(int iv=0;iv<nPrimV;iv++) {
    StMuPrimaryVertex* V= muMaker->muDst()->primaryVertex(iv);
    assert(V);
    if(V->ranking()<0.) continue; //drop vertices with too few matched tracks
    muMaker->muDst()->setVertexIndex(iv);
    const StThreeVectorF &r=V->position();
    const StThreeVectorF &er=V->posError();
    if(iv==0) hA[9]->Fill(r.z()); // histo only highe rank vertex
#if 0
    cout << "\nPrimary track " << nPrimTr << " momentum " << pr_track->p() << endl;  cout << "\t flag=" << pr_track->flag() << " nHits=" << pr_track->nHits()<< " vertID="<<  pr_track->vertexIndex()<< endl;
    cout << "\t primV("<<iv<<")  primDCA=" << pr_track->dca(iv) << ", pT="<< pr_track->pt()<< endl;
    if(pr_track->dca(iv).mag()>5) 	cout << "^^^^^ 3D DCA magnitude="<<pr_track->dca(iv).mag()<<endl;
    cout << "\t first point " << pr_track->firstPoint() << endl;
    cout << "\t last point " << pr_track->lastPoint() << endl;
#endif
      

    printf("iv=%d   Vz=%.2f +/-%.2f  bestZ=%f\n",iv,r.z(),er.z() ,zVert );
  
    if( fabs(zVert)< fabs(r.z()))  continue;
    zVert=r.z(); 
    iVert=iv;
  }

  //  printf("iVert=%d\n",iVert);
  if(iVert<0) return; // non of vertices has positive rank

  hA[1]->Fill(zVert);
  if(fabs(zVert) > cut_zVertex) return; // drop if too large Z 
  hA[0]->Fill(2);

  printf("pick zVert=%f iV=%d\n", zVert,iVert);

  muMaker->muDst()->setVertexIndex(iVert);  
  Int_t nPrimTrAll=muMaker->muDst()->GetNPrimaryTrack();
  for(int itr=0;itr<nPrimTrAll;itr++) {
    StMuTrack *pr_track=muMaker->muDst()->primaryTracks(itr);
    assert(pr_track->vertexIndex()==iVert);
    if(pr_track->flag()<=0) continue;
    if(pr_track->flag() != 301) continue; // use TPC-only tracks

    hA[0]->Fill(10);
    hA[2]->Fill(pr_track->pt());
    if(pr_track->pt()< cut_primPt) continue;	

    hA[0]->Fill(11);
    hA[3]->Fill(pr_track->eta());
    if(fabs(pr_track->eta())> cut_primEta) continue;

    hA[0]->Fill(12);
    float dedx=pr_track->dEdx()*1e6;
    float mom= pr_track->p().mag();
    // printf("xxx %f %f\n", dedx,mom);
    hA[8]->Fill(mom,dedx);
    
    if(dedx<1.5) continue;
    if(dedx>cut_dedx) continue;

    hA[0]->Fill(13);
    
    const StMuTrack* globTr= pr_track->globalTrack();
    
    assert(globTr);
    assert(globTr->flag()>0);
    float nFF=(1.*globTr->nHitsFit())/ globTr->nHitsPoss();
    hA[4]->Fill(nFF);
    if(nFF< cut_nFitFrac) continue;
    hA[0]->Fill(14);

    float Rxy=globTr->lastPoint().perp();
    hA[10]->Fill(Rxy);
    if(Rxy<cut_primRxy) continue;
    hA[0]->Fill(15);
 
    StPhysicalHelixD TrkHlx=globTr->outerHelix();
    
    const int mxPL=2; // # of Radii to be tested       
    float RxyA[mxPL]={225.4, 248.4};// cm
    
    int softID=-888;
    StThreeVectorD pos3D;
    float posPhi=888;
    for(int ipl=0;ipl<mxPL;ipl++) { //..... top & botom of the Barrel tile 
      float Rbprs= RxyA[ipl];
      pairD  d2; 
      d2 = TrkHlx.pathLength(Rbprs);
      printf(" path ipl=%d =%f, 2=%f, period=%f, trR=%f\n",ipl, d2.first ,d2.second,TrkHlx.period(),1./TrkHlx.curvature());
      if(d2.first>=0 || d2.second<=0) {
	LOG_WARN<< Form("extrapolateTrk , unexpected solution for track crossing BPRS\n  d2.firts=%f, second=%f, track ignored",  d2.first, d2.second)<<endm;
	continue;
      }
      
      pos3D = TrkHlx.at(d2.second);
      double xmagn = ::sqrt( pos3D.x()*pos3D.x() + pos3D.y()*pos3D.y() );
      printf(" punchBPRS x,y,z=%.1f, %.1f, %.1f, Rxy=%.1f  eta=%.2f\n",pos3D.x(),pos3D.y(),pos3D.z(),xmagn, globTr->eta());
      
      int softIDx;
      int ierr= mJanDbMaker->mBprsGeom->getId(pos3D.phi(),pos3D.pseudoRapidity(),softIDx);
      printf("hit tower id=%d, ierr=%d  ipl=%d\n",softIDx, ierr,ipl);
      
      posPhi=atan2(pos3D.y(),pos3D.x());
      if(posPhi<0) posPhi+=2*C_PI;	
      hA[5+ipl]->Fill(pos3D.z(),posPhi);       
      if(ierr) break; // do not try another depth for this track
      assert(softIDx>0 && softIDx<=mxBtow);

      if(ipl==0) { // BPRS layer
	hA[0]->Fill(16);
	if(!checkFiducial(pos3D.z(),posPhi,softIDx, Rbprs)) break; // to close to the edge     
	softID=softIDx;
	hA[0]->Fill(17);
      } else {
	hA[0]->Fill(18);
	if(softID!=softIDx) softID=-999;
	hA[0]->Fill(19);
	if(!checkFiducial(pos3D.z(),posPhi,softIDx, Rbprs)) softID=-999; // to close to the edge     
	// abort this track, it passes through 2 tiles
      }
    }
    
    if(softID<1) continue;
    nPrimTr++; 
    hA[0]->Fill(20);
    
    hA[14]->Fill(pr_track->pt());
    hA[15]->Fill(mom);    
    hA[7]->Fill(pos3D.z(),posPhi);       
    hA[12]->Fill(softID);

#if 1 
    // ..................try any possible BPRS mapping, if activated
    if(hA[16])
      for(int j=0; j<mxBtow; j++) {
	int ibp=kBPrs;  // switch: BTOW or BPRS - only one!
	if(fullEve.statTile[ibp][j]) continue; // skip masked tiles
	float adc=fullEve.adcTile[ibp][j];
	if(adc<7 ) continue;
	if(adc>35 ) continue;
	hA[16]->Fill(j+1,softID);
      }
#endif

    int id0=softID-1;
    uint isMip[mxBTile];
    uint badPed[mxBTile];
    float adc[mxBTile];
    for(int ibp=0;ibp<mxBTile;ibp++) {
      isMip[ibp]=0; // default =NO
      adc[ibp] =fullEve.adcTile [ibp][id0];
      badPed[ibp]=fullEve.statTile[ibp][id0];
      if( badPed[ibp]) continue;
      float adcL, adcH; // default, to use also tiles w/o gains
      mJanDbMaker->cut_mipAdcLH(ibp,softID,adcL,adcH);
      if(adc[ibp]<adcL) continue;
      if(adc[ibp]>adcH) continue;
      isMip[ibp]=1;
    }
 
    if(!badPed[kBPrs]) {  
      hA[0]->Fill(21);    
      hA[22]->Fill(softID,adc[kBPrs]);
      if(isMip[kBTow]) {
	hA[0]->Fill(22);    
	hA[23]->Fill(softID,adc[kBPrs]);
      }
    }

   if(!badPed[kBTow]) {  
      hA[0]->Fill(23);    
      hA[24]->Fill(softID,adc[kBTow]);
      if(isMip[kBPrs]) {
	hA[0]->Fill(24);    
	hA[25]->Fill(softID,adc[kBTow]);
      }
    }

   if(isMip[kBPrs] && isMip[kBTow]) 	hA[0]->Fill(25);


  } // end of loop over tracks
  
  if(nPrimTr<=0) return;

  hA[0]->Fill(3);
  hA[13]->Fill(zVert);
  if(nPrimTr>1) hA[0]->Fill(4);
}
   
//________________________________________________
//________________________________________________
int BarrelMipCalib::checkFiducial(float zTr, float phiTr, int softID, float Rxy){
  
  float etaHalfWidth=0.05/2.;
  const  float con_phiHalfWidth=(11.1/225.4)/2.; // radians
  float  cut_eps=cut_zMargin; // same for all except etaBin=20
  if((softID-1)%20==19)  cut_eps=cut_zMargin/2.; // be more forgiving

  float etaTw;
  assert( mJanDbMaker->mBprsGeom->getEta(softID,etaTw)==0);
 
  if(fabs(etaTw)<0.050) etaHalfWidth=(0.05-0.0035)/2.;
  if(fabs(etaTw)>0.95) etaHalfWidth=(0.9835-0.95)/2.;
 
  // those are edges of the tower in theta
  float thetaL=2.*atan(exp(-etaTw+etaHalfWidth));
  float thetaH=2.*atan(exp(-etaTw-etaHalfWidth));
 
  float zL=Rxy/tan(thetaL);
  float zH=Rxy/tan(thetaH);
		      
  float delZ1=zTr-zL;
  float delZ2=zH-zTr;
  bool isInZ= (delZ1>cut_eps) && (delZ2>cut_eps);
  printf("checkFiducial(zTr=%f phiTr=%f, id=%d  Rxy=%f\n",zTr,phiTr,softID,Rxy);
  printf(" theta range (deg)=%.1f %.1f\n",thetaL/3.1416*180,thetaH/3.1416*180); 
  printf("    etaTw=%f w2=%f  zL=%f zH=%f  delZ1=%f delZ2=%f isInZ=%d\n",etaTw,etaHalfWidth,zL,zH,delZ1, delZ2,isInZ);

 if(  isInZ) {
   hA[20]->Fill(delZ1);
   hA[20]->Fill(delZ2);
 }

  float phiTw;
  assert( mJanDbMaker->mBprsGeom->getPhi(softID,phiTw)==0);
  TVector2  uniTw; uniTw.SetMagPhi(1.,phiTw);
  TVector2  uniTr; uniTr.SetMagPhi(1.,phiTr);
  float delPhi=uniTw.DeltaPhi(uniTr);

  bool isInPhi=(con_phiHalfWidth- fabs(delPhi)) *Rxy> cut_eps;
		  
  printf("  phiTr=%f phiTw=%f delPhi=%f  delRphi=%f isInPhi=%d\n", phiTr, phiTw,delPhi, delPhi*Rxy, isInPhi);
 if(isInPhi) hA[21]->Fill(delPhi *Rxy);

  return isInZ && isInPhi ;

}
  

// test code to find where in eta are towers at eat=1.0
//________________________________________________
//________________________________________________
void BarrelMipCalib::searchEtaBin20(  JanBarrelEvent &fullEve){

  // Access to muDst .......................
  StMuEvent* muEve = muMaker->muDst()->event();
  int nPrimV=muMaker->muDst()->numberOfPrimaryVertices();
  hA[0]->Fill(0);
  
  printf("\nWARN-ALTERNATIVE CODE: mipCalib eventID %d nPrimV=%d  =============\n", muEve->eventId(),nPrimV);

  // ..............filter events , keep only those with MIP tower at eta=1.0

  int pickBin=17;
  int myId0=-1;
  for(int j=0; j<mxBtow; j++) {
    if((j%20)!=pickBin-1) continue; // fixed eta-bin
    if((j/20)%3) continue; // every 3rd phi- row

    int ibp=kBTow;  // switch: BTOW or BPRS - only one!
    if(fullEve.statTile[ibp][j]) continue; // skip masked tiles
    float adc=fullEve.adcTile[ibp][j];
    if(adc<10 ) continue;
    if(adc>30 ) continue;
    myId0=j;
    break;
  }
  if(myId0<0) return;
  hA[0]->Fill(1);
  float phiTw;
  assert( mJanDbMaker->mBprsGeom->getPhi(myId0+1,phiTw)==0);
  TVector2  uniTw; uniTw.SetMagPhi(1.,phiTw);

  if(nPrimV<=0) return;
  hA[0]->Fill(2);

  
  int nPrimTr =0;
  
  // find vertex with smalest Z - to compensate for PPV error in FMS-production

  int iVert=-1;
  float zVert=99999;
  
  for(int iv=0;iv<nPrimV;iv++) {
    StMuPrimaryVertex* V= muMaker->muDst()->primaryVertex(iv);
    assert(V);
    if(V->ranking()<0.) continue; //drop vertices with too few matched tracks
    muMaker->muDst()->setVertexIndex(iv);
    const StThreeVectorF &r=V->position();
    const StThreeVectorF &er=V->posError();
    if(iv==0) hA[9]->Fill(r.z()); // histo only highe rank vertex

    printf("iv=%d   Vz=%.2f +/-%.2f  bestZ=%f\n",iv,r.z(),er.z() ,zVert );
  
    if( fabs(zVert)< fabs(r.z()))  continue;
    zVert=r.z(); 
    iVert=iv;
  }

  //  printf("iVert=%d\n",iVert);
  if(iVert<0) return; // non of vertices has positive rank

  hA[1]->Fill(zVert);
  if(fabs(zVert) > cut_zVertex) return; // drop if too large Z 
  hA[0]->Fill(3);

  printf("pick zVert=%f iV=%d\n", zVert,iVert);

  muMaker->muDst()->setVertexIndex(iVert);  
  Int_t nPrimTrAll=muMaker->muDst()->GetNPrimaryTrack();
  for(int itr=0;itr<nPrimTrAll;itr++) {
    StMuTrack *pr_track=muMaker->muDst()->primaryTracks(itr);
    assert(pr_track->vertexIndex()==iVert);
    if(pr_track->flag()<=0) continue;

    hA[0]->Fill(10);
    hA[2]->Fill(pr_track->pt());
    if(pr_track->pt()< cut_primPt) continue;	

    hA[0]->Fill(11);
    hA[3]->Fill(pr_track->eta());
    if(fabs(pr_track->eta())> cut_primEta) continue;

    hA[0]->Fill(12);
    float dedx=pr_track->dEdx()*1e6;
    float mom= pr_track->p().mag();
    // printf("xxx %f %f\n", dedx,mom);
    hA[8]->Fill(mom,dedx);
    
    if(dedx<1.5) continue;
    if(dedx>cut_dedx) continue;

    hA[0]->Fill(13);
    
    const StMuTrack* globTr= pr_track->globalTrack();
    
    assert(globTr);
    assert(globTr->flag()>0);
    float nFF=(1.*globTr->nHitsFit())/ globTr->nHitsPoss();
    hA[4]->Fill(nFF);
    if(nFF< cut_nFitFrac) continue;
    
    hA[0]->Fill(14);
    StPhysicalHelixD TrkHlx=globTr->outerHelix();
    
    const int mxPL=2; // # of Radii to be tested       
    float RxyA[mxPL]={225.4, 248.4};// cm
    
    int softID=-888;
    StThreeVectorD pos3D;
    float posPhi=888;
    for(int ipl=0;ipl<1;ipl++) { //..... top & botom of the Barrel tile 
      float Rbprs= RxyA[ipl];
      pairD  d2; 
      d2 = TrkHlx.pathLength(Rbprs);
      printf(" path ipl=%d =%f, 2=%f, period=%f, trR=%f\n",ipl, d2.first ,d2.second,TrkHlx.period(),1./TrkHlx.curvature());
      if(d2.first>=0 || d2.second<=0) {
	LOG_WARN<< Form("extrapolateTrk , unexpected solution for track crossing BPRS\n  d2.firts=%f, second=%f, track ignored",  d2.first, d2.second)<<endm;
	continue;
      }
      
      pos3D = TrkHlx.at(d2.second);
      double xmagn = ::sqrt( pos3D.x()*pos3D.x() + pos3D.y()*pos3D.y() );
      printf(" punchBPRS x,y,z=%.1f, %.1f, %.1f, Rxy=%.1f  eta=%.2f\n",pos3D.x(),pos3D.y(),pos3D.z(),xmagn, globTr->eta());
      
      posPhi=atan2(pos3D.y(),pos3D.x());
      if(posPhi<0) posPhi+=2*C_PI;	
      hA[5+ipl]->Fill(pos3D.z(),posPhi);    
   
      TVector2  uniTr; uniTr.SetMagPhi(1.,posPhi);
      float delPhi=uniTw.DeltaPhi(uniTr);

      if(fabs(delPhi)>0.15) continue;
      softID=111; // take every track
    }
    
    if(softID<1) continue;
    nPrimTr++; 
    hA[0]->Fill(19);
    
    hA[14]->Fill(pr_track->pt());
    hA[15]->Fill(mom);    
    hA[7]->Fill(pos3D.z(),posPhi);       
    hA[17]->Fill(pos3D.z());
    if(pos3D.z()>0) hA[18]->Fill(posPhi);       
    //  hA[12]->Fill(softID);
    
  } // end of loop over tracks
  
  if(nPrimTr<=0) return;
  
  hA[0]->Fill(4);
  hA[13]->Fill(zVert);
  if(nPrimTr>1) hA[0]->Fill(5);
}
   
  
    
