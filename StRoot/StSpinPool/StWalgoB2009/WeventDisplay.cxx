// $Id: WeventDisplay.cxx,v 1.6 2010/01/10 03:01:37 balewski Exp $
//
//*-- Author : Jan Balewski, MIT


#include <TH1.h>
#include <TH2.h>
#include <TMath.h>
#include <TCanvas.h>
#include <TStyle.h>
#include <TFile.h>
#include <TText.h>
#include <TList.h>
#include <TBox.h>
#include <TPaveText.h>
#include <TPad.h>
#include <TEllipse.h>
#include <stdio.h>

//MuDst
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuTriggerIdCollection.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuTrack.h>
#include <StMuDSTMaker/COMMON/StMuPrimaryVertex.h>
#include "StEmcUtil/geometry/StEmcGeom.h"

#include "St2009WMaker.h"
#include "WanaConst.h"
#include "WeventDisplay.h"
//-----------------------------
//-----------------------------
WeventDisplay::WeventDisplay( St2009WMaker* mk, int mxEv) {
  maxEve=mxEv;
  wMK=mk;
  const float PI=TMath::Pi();
  const char cPlane[ mxBSmd]={'E','P'};
  char txt1[100], txt2[1000];

  // barrel
  etaBL_ln=new TLine(-1,-3.2,1,3.2); etaBL_ln->SetLineColor(kBlue);
  etaBR_ln=new TLine(-1,-3.2,1,3.2); etaBR_ln->SetLineColor(kBlue);

  etaEL_ln=new TLine(2,-3.2,3,3.2); etaEL_ln->SetLineColor(kGreen);
  etaER_ln=new TLine(2,-3.2,3,3.2); etaER_ln->SetLineColor(kGreen);

  bxT=new TBox(-1.3,0, 1.3,1.);  bxT->SetFillStyle(0); bxT->SetLineStyle(2);
  bxE=new TBox(-1.3,0, 1,2.);  bxE->SetFillStyle(0); bxE->SetLineStyle(2);
  if(wMK->par_useEtow>=2) bxE->SetX2(2.2);


  hEmcET=new TH2F("eveBtowET","EMC  ET sum, Z=[0.3,30]GeV; event eta ; phi",38,-1.4,2.4,63,-PI,PI);

  hTpcET=new TH2F("eveTpcET","TPC PT sum, Z[0.3,10]GeV/c; event eta ; phi",28,-1.4,1.4,63,-PI,PI);

  for(int iep=0;iep<mxBSmd;iep++) {
    sprintf(txt1,"eveBsmdAdc_%c",cPlane[iep]);
    sprintf(txt2,"BSMD_%c  ADC sum Z=[30,1000]; event eta ; phi",cPlane[iep]);
    hBsmdAdc[iep]=new TH2F(txt1,txt2,26,-1.3,1.3,63,-PI,PI);
  }

  
#if 0
  //---- smart code from Willie
  float etabinsA[1+mxBetaStrMod*2], etaphibinsA[mxBMod2Pi+1];
  for(int i=0;i<mxBetaStrMod+1;i++) 
    etabinsA[mxBetaStrMod-i]=-(etabinsA[mxBetaStrMod+i]=wMK->mSmdEGeom->EtaB()[i]);
  for(int i=0;i<mxBMod2Pi+1;i++) 
    etaphibinsA[i]=wMK->mSmdEGeom->PhiB()[i];
  hBsmdEtaAdc=new TH2F("eveBsmdEtaAdc"," Event: BSMD-Eta ADC vs. eta & phi; pseudorapidity; azimuth",mxBetaStrMod*2,etabinsA,mxBMod2Pi,etaphibinsA);
#endif  
  
}



//-----------------------------
//-----------------------------
void
WeventDisplay::clear(){
  hEmcET->Reset();
  hTpcET->Reset();
  for(int iep=0;iep<mxBSmd;iep++)  hBsmdAdc[iep]->Reset();

}

//-----------------------------
//-----------------------------
void
WeventDisplay::draw(  const char *tit,int eveID, int daqSeq,  int runNo,  WeveVertex myV, WeveEleTrack myTr){
  if(maxEve<=0) return;
  maxEve--;
  TStyle *myStyle=new TStyle();
  myStyle->cd();
  myStyle->SetPalette(1,0);
  myStyle->SetOptStat(1000010);
  
  char txt[1000];
  sprintf(txt,"display-%s_run%d.eventId%06dvert%d",tit,runNo,eveID,myV.id);
 
  printf("WeventDisplay::Draw %s\n",txt);
  TCanvas *c0=new TCanvas(txt,txt,850,600);
  c0->cd();

  TString tt=txt;
  TPad *cU = new TPad(tt+"U", tt+"U",0.,0.2,1.,1.); cU->Draw();
  TPad *cD = new TPad(tt+"D", tt+"D",0.,0.,1.,0.2); cD->Draw();

  cU->cd();
  TPad *cU1 = new TPad(tt+"U1", tt+"U1",0.,0.,0.24,1.); cU1->Draw();
  TPad *cU2 = new TPad(tt+"U2", tt+"U2",0.24,0.,0.55,1.); cU2->Draw();
  TPad *cU3 = new TPad(tt+"U3", tt+"U3",0.55,0.,1.,1.);  cU3->Draw();
  cU3->Divide(2,1);
 

  cU1->cd(); hTpcET->Draw("colz");
     
  TVector3 rW=myTr.pointTower.R;
  rW.SetZ(rW.z()-myV.z);

  TEllipse *te1=new TEllipse(rW.Eta(),rW.Phi(), 0.1,0.1);
  te1->SetFillStyle(0);te1->SetLineStyle(3); te1->SetLineColor(kMagenta);
  TEllipse *te2=new TEllipse(rW.Eta(),rW.Phi(), 0.7, 0.7);
  te2->SetFillStyle(0);te2->SetLineStyle(3); te2->SetLineColor(kBlack);

  TVector3 rA=-rW; // away direction
  bxT->SetY1(rA.Phi() - wMK->par_awayDeltaPhi);
  bxT->SetY2(rA.Phi() + wMK->par_awayDeltaPhi);

  bxE->SetY1(rA.Phi() - wMK->par_awayDeltaPhi);
  bxE->SetY2(rA.Phi() + wMK->par_awayDeltaPhi);


  te1->Draw();   te2->Draw(); bxT->Draw("l");
  etaBL_ln->Draw();  etaBR_ln->Draw(); etaEL_ln->Draw();
  
  cU2->cd();    hEmcET->Draw("colz");
  te1->Draw();  te2->Draw(); bxE->Draw("l");
  etaBL_ln->Draw();  etaBR_ln->Draw();
  etaEL_ln->Draw();  etaER_ln->Draw();
  
  for(int iep=0;iep<mxBSmd;iep++) {
    cU3->cd(1+iep);
    hBsmdAdc[iep]->Draw("colz");
    te1->Draw();  te2->Draw();  bxT->Draw("l");
    etaBL_ln->Draw();  etaBR_ln->Draw();
  }

  //........... text information .............
  TPaveText *pvt = new TPaveText(0,0.,1,1,"br");
  cD->cd();
  sprintf(txt,"run=%d  eveID=%05d daq=%d vertex:ID=%d Z=%.0fcm",runNo,eveID,daqSeq,myV.id,myV.z);
  printf("WeventDisplay::Event ID  %s\n",txt);
  pvt->AddText(txt);

  sprintf(txt,"TPC PT(GeV/c) near=%.1f  away=%.1f ", myTr.nearTpcPT, myTr.awayTpcPT);
  printf("WeventDisplay::Event TPC  %s\n",txt);
  pvt->AddText(txt);

  sprintf(txt,"BTOW ET/GeV: 2x2=%.1f   near= %.1f   away= %.1f",myTr.cluster.ET,myTr.nearBtowET,myTr.awayBtowET);
  printf("WeventDisplay:: BTOW  %s\n",txt);
  pvt->AddText(txt);

  sprintf(txt,"Emc (Btow+Etow) ET/GeV:   near= %.1f   away= %.1f",myTr.nearEmcET,myTr.awayEmcET);
  printf("WeventDisplay:: BTOW+ETOW  %s\n",txt);
  pvt->AddText(txt);

  sprintf(txt,"total ET/GeV:   near= %.1f   away= %.1f  ptBalance= %.1f",myTr.nearTotET,myTr.awayTotET,myTr.ptBalance.Perp());
  printf("WeventDisplay:: BTOW  %s\n",txt);
  pvt->AddText(txt);


  pvt->Draw();

  c0->Print();
  // save dump of histos
  sprintf(txt,"display-%s_run%d.eventId%05dvert%d.root",tit,runNo,eveID,myV.id);
  TFile hf(txt,"recreate");
  if(hf.IsOpen()) {
    hEmcET->Write();
    hTpcET->Write();
    for(int iep=0;iep<mxBSmd;iep++) hBsmdAdc[iep]->Write();
    hf.Close();
  }
}

//-----------------------------
//-----------------------------
void
WeventDisplay::exportEvent( const char *tit, WeveVertex myV, WeveEleTrack myTr){
  if(maxEve<=0) return;
  clear();
  int eveId=wMK->mMuDstMaker->muDst()->event()->eventId();
  int runNo=wMK->mMuDstMaker->muDst()->event()->runId();
  const char *afile = wMK->mMuDstMaker->GetFile();
  int len=strlen(afile);
  int daqSeq=atoi(afile+(len-18));
  //  printf("DDD %s len=%d %d =%s=\n",afile,len,daqSeq,afile+(len-15));

  TVector3 rTw=myTr.cluster.position;
  rTw.SetZ(rTw.z()-myV.z);
  printf("#xcheck-%s run=%d daqSeq=%d eveID=%7d vertID=%2d zVert=%.1f prTrID=%4d  prTrEta=%.3f prTrPhi/deg=%.1f globPT=%.1f hitTwId=%4d twAdc=%.1f clEta=%.3f clPhi/deg=%.1f  clET=%.1f\n",tit,
	 runNo,daqSeq,eveId,myV.id,myV.z,
	 myTr.prMuTrack->id(),myTr.prMuTrack->eta(),myTr.prMuTrack->phi()/3.1416*180.,myTr.glMuTrack->pt(),
	 myTr.pointTower.id,wMK->wEve.bemc.adcTile[kBTow][myTr.pointTower.id-1],
	 rTw.Eta(),rTw.Phi()/3.1416*180.,myTr.cluster.ET);

  float zVert=myV.z;
  printf("WeventDisplay-%s::export run=%d eve=%d\n",tit,runNo,eveId);

 //.... process BTOW hits
  for(int i=0;i< mxBtow;i++) {
    float ene=wMK->wEve.bemc.eneTile[kBTow][i];
    if(ene<=0) continue;
    TVector3 primP=wMK->positionBtow[i]-TVector3(0,0,zVert);
    primP.SetMag(ene); // it is 3D momentum in the event ref frame
    float ET=primP.Perp();

    float eveEta=primP.Eta();
    float evePhi=primP.Phi();
    hEmcET->Fill(eveEta,evePhi,ET); 
  }
  
  int par_useEtow = wMK->par_useEtow; //get ETOW flag   
  //.... store ETOW hits    
  if(par_useEtow >= 1) {
    for(int i=0;i<mxEtowPhiBin;i++){
      for(int j=0;j<mxEtowEta;j++){
	float ene=wMK->wEve.etow.ene[i][j];
	if(ene<=0) continue;
	TVector3 primP=wMK->positionEtow[i][j]-TVector3(0,0,zVert);
	primP.SetMag(ene); // it is 3D momentum in the event ref frame
	float ET=primP.Perp();
	
	float eveEta=primP.Eta();
	float evePhi=primP.Phi();
	hEmcET->Fill(eveEta,evePhi,ET);	
      }
    }
  }

  hEmcET->SetMinimum(0.3);  hEmcET->SetMaximum(30.);
 // compute approximate event eta for barrel
  float x,y,z;
  float Rcylinder= wMK->mBtowGeom->Radius();
  assert(wMK->mBtowGeom->getXYZ(20,x,y,z)==0); // this is approximate Z of last tower
  TVector3 rL(Rcylinder,0,z+myV.z);
  TVector3 rR(Rcylinder,0,z-myV.z);
  float etaL=-rL.Eta(), etaR=rR.Eta();
  etaBL_ln->SetX1(etaL);  etaBL_ln->SetX2(etaL);
  etaBR_ln->SetX1(etaR);  etaBR_ln->SetX2(etaR);
  if(wMK->par_useEtow<2) bxE->SetX2(etaR+0.05); // skip endcap
  
  // compute approximate event eta for Endcap
  rL=TVector3(0,214,270-myV.z); // detector eta~1.06
  rR=TVector3(0,77,270-myV.z); // detector eta 2.0
  etaL=rL.Eta(); etaR=rR.Eta();
  etaEL_ln->SetX1(etaL);  etaEL_ln->SetX2(etaL);
  etaER_ln->SetX1(etaR);  etaER_ln->SetX2(etaR);


  //... TPC
  hTpcET->SetMinimum(0.3);hTpcET->SetMaximum(10.);
  getPrimTracks( myV.id);
  
  //.... BSMD-Eta, -Phi
  
  for(int iep=0;iep<mxBSmd;iep++) {
    hBsmdAdc[iep]->SetMinimum(30);hBsmdAdc[iep]->SetMaximum(999);
    for(int i=0;i< mxBStrips;i++) {
      float adc=wMK->wEve.bemc.adcBsmd[iep][i];
      if(adc<=0) continue;
      TVector3 r=wMK->positionBsmd[iep][i];
      float z1=r.z()-zVert;
      r.SetZ(z1);
      hBsmdAdc[iep]->Fill(r.Eta(),r.Phi(),adc);
    }
  }// end of eta,phi-planes

  //.... produce plot & save
  draw(tit,eveId, daqSeq,runNo,myV, myTr);
  export2sketchup(tit,myV, myTr);
}

//-----------------------------
//-----------------------------
void
WeventDisplay::getPrimTracks( int vertID) {
  assert(vertID>=0);
  assert(vertID<(int)wMK->mMuDstMaker->muDst()->numberOfPrimaryVertices());
  StMuPrimaryVertex* V=wMK-> mMuDstMaker->muDst()->primaryVertex(vertID);
  assert(V);
  wMK-> mMuDstMaker->muDst()->setVertexIndex(vertID);
  float rank=V->ranking();
  assert(rank>0);
  
  Int_t nPrimTrAll=wMK->mMuDstMaker->muDst()->GetNPrimaryTrack();
  for(int itr=0;itr<nPrimTrAll;itr++) {
    StMuTrack *prTr=wMK->mMuDstMaker->muDst()->primaryTracks(itr);
    if(prTr->flag()<=0) continue;
    if(prTr->flag()!=301) continue;// TPC-only regular tracks
    float hitFrac=1.*prTr->nHitsFit()/prTr->nHitsPoss();
    if(hitFrac<wMK->par_nHitFrac) continue;
    StThreeVectorF prPvect=prTr->p();
    TVector3 primP=TVector3(prPvect.x(),prPvect.y(),prPvect.z());
    float pT=prTr->pt();
    hTpcET->Fill(prTr->eta(),prTr->phi(),pT);

  }
}


//-----------------------------
//-----------------------------
void
WeventDisplay::export2sketchup(  const char *tit, WeveVertex myV, WeveEleTrack myTr){
  int eveId=wMK->mMuDstMaker->muDst()->event()->eventId();
  int runNo=wMK->mMuDstMaker->muDst()->event()->runId();
  char txt[1000];
  sprintf(txt,"display3D-%s_run%d.eventId%05d_vert%d.txt",tit,runNo,eveId,myV.id);
  FILE *fd=fopen(txt,"w"); assert(fd);
  
  //........ DUMP PRIM TRACKS..........
  int vertID=myV.id;
  assert(vertID>=0);
  assert(vertID<(int)wMK->mMuDstMaker->muDst()->numberOfPrimaryVertices());
  StMuPrimaryVertex* V=wMK-> mMuDstMaker->muDst()->primaryVertex(vertID);
  assert(V);
  wMK-> mMuDstMaker->muDst()->setVertexIndex(vertID);
  float rank=V->ranking();
  assert(rank>0);
  const StThreeVectorF &rV=V->position();
  Int_t nPrimTrAll=wMK->mMuDstMaker->muDst()->GetNPrimaryTrack();
  for(int itr=0;itr<nPrimTrAll;itr++) {
    StMuTrack *prTr=wMK->mMuDstMaker->muDst()->primaryTracks(itr);
    if(prTr->flag()<=0) continue;
    if(prTr->flag()!=301) continue;// TPC-only regular tracks
    float hitFrac=1.*prTr->nHitsFit()/prTr->nHitsPoss();
    if(hitFrac<wMK->par_nHitFrac) continue;
    prTr->p();
    fprintf(fd,"track V %.1f %.3f %.3f  primP:PT:eta:phi:Q %.1f %.3f  %.3f  %d\n",rV.x(),rV.y(),rV.z(), prTr->p().perp(),prTr->p().pseudoRapidity(),prTr->p().phi(),prTr->charge());
  }

  //........DUMP BTOW towers
  float Rcylinder= wMK->mBtowGeom->Radius(), Rcylinder2=Rcylinder*Rcylinder;
  for(int i=0;i< mxBtow;i++) {
    float ene=wMK->wEve.bemc.eneTile[kBTow][i];
    if(ene<=0) continue;
    float delZ=wMK->positionBtow[i].z()-myV.z;
    float e2et=Rcylinder/sqrt(Rcylinder2+delZ*delZ);
    float ET=ene*e2et;    
    float detEta=wMK->positionBtow[i].Eta();
    float detPhi=wMK->positionBtow[i].Phi();
    fprintf(fd,"btow V %.1f %.3f %.3f  eveET:detEta:detPhi %.3f %.3f  %.3f\n",rV.x(),rV.y(),rV.z(),ET, detEta,detPhi);
  }
  
  const char cPlane[ mxBSmd]={'E','P'};
  //........DUMP BSMD  hits 
  for(int iep=0;iep<mxBSmd;iep++) {
    for(int i=0;i< mxBStrips;i++) {
      float adc=wMK->wEve.bemc.adcBsmd[iep][i];
      if(adc<=0) continue;
      TVector3 r=wMK->positionBsmd[iep][i];
      fprintf(fd,"bsmd%c V %.1f %.3f %.3f  adc:detEta:detPhi %.3f %.3f  %.3f\n",cPlane[iep],rV.x(),rV.y(),rV.z(),adc,r.Eta(),r.Phi() );
    }
  }

  //........DUMP ETOW towers
  for(int iphi=0; iphi<mxEtowPhiBin; iphi++){
    for(int ieta=0; ieta<mxEtowEta; ieta++){//sum all eta rings
      float ene=wMK->wEve.etow.ene[iphi][ieta];
      if(ene<=0) continue; //skip towers with no energy
      TVector3 detP=wMK->positionEtow[iphi][ieta];
      TVector3 primP=detP-TVector3(0,0,myV.z);
      primP.SetMag(ene); // it is 3D momentum in the event ref frame
      fprintf(fd,"etow V %.1f %.3f %.3f  eveET:detEta:detPhi %.3f %.3f  %.3f\n",rV.x(),rV.y(),rV.z(),primP.Perp(), detP.Eta(),detP.Phi());
    }
  }
  
  //.... DUMP reco electron ...
  float eleET=myTr.cluster.ET;
  fprintf(fd,"recoElectron V %.1f %.3f %.3f  ET:detEta:detPhi %.3f %.3f  %.3f\n",rV.x(),rV.y(),rV.z() , eleET,myTr.pointTower.R.Eta(),myTr.pointTower.R.Phi()); 

  
  //... close event file
  fclose(fd);


}


// $Log: WeventDisplay.cxx,v $
// Revision 1.6  2010/01/10 03:01:37  balewski
// cleanup & nicer histos
//
// Revision 1.5  2010/01/09 00:07:16  stevens4
// add jet finder
//
// Revision 1.4  2010/01/06 19:16:48  stevens4
// track cuts now on primary component, cleanup
//
// Revision 1.3  2010/01/06 04:22:15  balewski
// added Q/PT plot for Zs, more cleanup
//
// Revision 1.2  2009/12/10 16:01:31  stevens4
// fixed zero-length vector
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
