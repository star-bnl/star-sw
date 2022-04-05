// $Id: WeventDisplay.cxx,v 1.6 2012/09/21 16:59:10 balewski Exp $
//
//*-- Author : Jan Balewski, MIT


#include <TH1.h>
#include <TH2.h>
#include <TMath.h>
#include <TCanvas.h>
#include <TStyle.h>
#include <TFile.h>
#include <TText.h>
#include <TLatex.h>
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

//Esmd geometry for plots
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"

#include "St2011WMaker.h"
#include "WanaConst.h"
#include "WeventDisplay.h"
//-----------------------------
//-----------------------------
WeventDisplay::WeventDisplay( St2011WMaker* mk, int mxEv) {
  maxEve=mxEv;
  wMK=mk;
  const float PI=TMath::Pi();
  const char cPlane[ mxBSmd]={'E','P'};
  const char cEsmdPlane[ mxEsmdPlane]={'U','V'};
  char txt1[100], txt2[1000];

  // barrel
  etaBL_ln=new TLine(-1,-3.2,1,3.2); etaBL_ln->SetLineColor(kBlue);
  etaBR_ln=new TLine(-1,-3.2,1,3.2); etaBR_ln->SetLineColor(kBlue);

  etaEL_ln=new TLine(2,-3.2,3,3.2); etaEL_ln->SetLineColor(kGreen);
  etaER_ln=new TLine(2,-3.2,3,3.2); etaER_ln->SetLineColor(kGreen);

  bxT=new TBox(-1.3,0, 1.3,1.);  bxT->SetFillStyle(0); bxT->SetLineStyle(2);
  bxE=new TBox(-1.3,0, 1,2.);  bxE->SetFillStyle(0); bxE->SetLineStyle(2);
  bxE->SetX2(2.2);


  hEmcET=new TH2F("eveBtowET","EMC  ET sum, Z=[0.3,30]GeV; event eta ; phi",38,-1.4,2.4,63,-PI,PI);

  hTpcET=new TH2F("eveTpcET","TPC PT sum, Z[0.3,10]GeV/c; event eta ; phi",32,-1.6,1.6,63,-PI,PI);

  for(int iep=0;iep<mxBSmd;iep++) {
    sprintf(txt1,"eveBsmdAdc_%c",cPlane[iep]);
    sprintf(txt2,"BSMD_%c  ADC sum Z=[30,1000]; event eta ; phi",cPlane[iep]);
    hBsmdAdc[iep]=new TH2F(txt1,txt2,26,-1.3,1.3,63,-PI,PI);
  }

  //ESMD shower shape
  for(int iuv=0; iuv<mxEsmdPlane; iuv++){
    sprintf(txt1,"eveEsmdShower_%c",cEsmdPlane[iuv]);
    sprintf(txt2,"ESMD_%c Shower Shape; i_strip position - track position (cm) ; MeV",cEsmdPlane[iuv]);
    hEsmdShower[iuv]=new TH1F(txt1,txt2,41,-10.25,10.25);
    bxEs[iuv]=new TBox(-5,0, 3,20.);  bxEs[iuv]->SetFillStyle(3002); bxEs[iuv]->SetFillColor(kYellow);

  }
  hEsmdXpt=new TH2F("eveEsmdXpt","ESMD Cross Point; X (cm); Y (cm)",100,0.,100,100,0.,100.);

  
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
  for(int iuv=0;iuv<mxEsmdPlane;iuv++)  hEsmdShower[iuv]->Reset();
  hEsmdXpt->Reset();

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
  TCanvas *c0; TPaveText *pvt;
  string detector=tit;

  if(detector.compare("WB") == 0){ //barrel event display
    sprintf(txt,"display-%s%.0f_run%d.eventId%05dvert%d",tit,myTr.cluster.ET,runNo,eveID,myV.id);
    TFile hf(Form("%s.root",txt),"recreate"); //TFile hf(txt,"recreate");
    c0=new TCanvas(txt,txt,850,600);
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
    pvt = new TPaveText(0,0.,1,1,"br");
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
    // save dump of histos
    if(hf.IsOpen()) {
      hEmcET->Write();
      hTpcET->Write();
      for(int iep=0;iep<mxBSmd;iep++) hBsmdAdc[iep]->Write();
      hf.Close();
    }
  }
  else if(detector.compare("WE") == 0){ //endcap event display
    sprintf(txt,"display-%s%.0f_run%d.eventId%05dvert%d",tit,myTr.cluster.ET,runNo,eveID,myV.id);
    TFile hf(Form("%s.root",txt),"recreate");
    c0=new TCanvas(txt,txt,1750,1300);
    c0->cd();

    TString tt=txt;
    TPad *cL = new TPad(tt+"L", tt+"L",0.,0.,0.6,1.); cL->Draw();
    TPad *cR = new TPad(tt+"R", tt+"R",0.6,0.,1.,1.); cR->Draw();
    cL->cd();
    TPad *cLU = new TPad(tt+"LU", tt+"LU",0.,0.2,1.,1.); cLU->Draw();
    TPad *cLD = new TPad(tt+"LD", tt+"LD",0.,0.,1.,0.2); cLD->Draw();
    cLU->cd();
    TPad *cLU1 = new TPad(tt+"LU1", tt+"LU1",0.,0.,0.44,1.); cLU1->Draw();
    TPad *cLU2 = new TPad(tt+"LU2", tt+"LU2",0.44,0.,1.,1.); cLU2->Draw();
    cR->cd();
    TPad *cRU = new TPad(tt+"RU", tt+"RU",0.,0.5,1.,1.); cRU->Draw();
    TPad *cRD = new TPad(tt+"RD", tt+"RD",0.,0.,1.,0.5); cRD->Draw();
    cRD->Divide(1,2);
    cLU1->cd(); hTpcET->Draw("colz");
    
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
    
    cLU2->cd();    hEmcET->Draw("colz");
    te1->Draw();  te2->Draw(); bxE->Draw("l");
    etaBL_ln->Draw();  etaBR_ln->Draw();
    etaEL_ln->Draw();  etaER_ln->Draw();
    
    TList *Lx;  TLine *tline; TLine *tlineGlob;
    //Draw ESMD showers
    for(int iuv=0; iuv<mxEsmdPlane; iuv++){
      cRD->cd(1+iuv);

      Lx=hEsmdShower[iuv]->GetListOfFunctions();
      tline=new TLine(myTr.esmdDca[iuv],0.,myTr.esmdDca[iuv],100.);
      tline->SetLineColor(2);
      tlineGlob=new TLine(myTr.esmdGlobStrip[iuv]*0.5+myTr.esmdDcaGlob[iuv],0,myTr.esmdGlobStrip[iuv]*0.5+myTr.esmdDcaGlob[iuv],100.); tlineGlob->SetLineColor(2);
      hEsmdShower[iuv]->Draw();
      tline->Draw(); Lx->Add(tline);
      tlineGlob->SetLineStyle(2); tlineGlob->Draw(); Lx->Add(tlineGlob);
      hEsmdShower[iuv]->Draw();

      bxEs[iuv]->SetX1( 0.5*(-wMK->parE_esmdGL+myTr.esmdPeakOffset[iuv]) -0.25);
      bxEs[iuv]->SetX2( 0.5*(+wMK->parE_esmdGL+myTr.esmdPeakOffset[iuv]+1)-0.25);
      bxEs[iuv]->SetY2( myTr.esmdPeakSumE[iuv]/(2*wMK->parE_esmdGL+1));
      bxEs[iuv]->Draw();

      //print Q/Pt warning
      if( iuv==1 && myTr.prMuTrack->pt() >= 100.0 ) {
	TLatex* tx = new TLatex(3,0.9*hEsmdShower[iuv]->GetMaximum(),"| Q/P_{T} | < 0.01");  tx->SetTextColor(kRed); tx->SetTextSize(0.1); tx->Draw();
      }
    }
    hEsmdShower[0]->SetFillColor(kBlue-5);
    hEsmdShower[1]->SetFillColor(kGreen-5);

    //make SMD cluster picture
    cRU->cd();
    hEsmdXpt->Draw("colz");
    //fill with lines of hit strips
    int secLoop[3]={myTr.hitSector-2,myTr.hitSector-1,myTr.hitSector};
    if(secLoop[0] < 0) secLoop[0]=11;
    if(secLoop[2] > 11) secLoop[2]=0;
    for(int iuv=0; iuv<mxEsmdPlane; iuv++){
      for(int isec=0; isec<3; isec++){
	for(int k=0; k<288; k++){ //loop all strips
	  const StructEEmcStrip *stripPtr = wMK->geoSmd->getStripPtr(k,iuv,secLoop[isec]); //myTr.esmdStripId[iuv][k]
	  if(!stripPtr) continue;
	  if(wMK->wEve->esmd.ene[secLoop[isec]][iuv][k]*1e3 < 10.) continue;
	  int nColor = (int) (wMK->wEve->esmd.ene[secLoop[isec]][iuv][k]*1e3)/20;
	  int nSub=-10;
	  if(nColor==0) nSub=10;
	  if(nColor==1) nSub=8;
	  if(nColor==2) nSub=5;
	  if(nColor==3) nSub=1;
	  if(nColor==4) nSub=-4;
	  TVector3 end1=stripPtr->end1;
	  TVector3 end2=stripPtr->end2;
	  Lx=hEsmdXpt->GetListOfFunctions();
	  tline=new TLine(end1.X(),end1.Y(),end2.X(),end2.Y());
	  if(iuv==0) tline->SetLineColor(kBlue-nSub);
	  if(iuv==1) tline->SetLineColor(kGreen-nSub);
	  if(wMK->wEve->esmd.adc[secLoop[isec]][iuv][k] > 3896) tline->SetLineColor(2); //saturation with ped=200
	  tline->Draw(); Lx->Add(tline);
	}
      }
    }

    TEllipse *te3=new TEllipse(rW.X(),rW.Y(), 2.0, 2.0);
    te3->SetLineColor(kBlue); te3->SetFillStyle(0); te3->Draw(); //track projection
    //TEllipse *te5=new TEllipse(myTr.pointTower.Rglob.X(),myTr.pointTower.Rglob.Y(), .25, .25);
    //te5->SetLineColor(kBlue); te5->Draw(); //global track projection
    TEllipse *te4=new TEllipse(0.,0., 215., 215.);
    te4->SetFillStyle(0); te4->SetLineColor(kBlue); te4->Draw("l"); //outer esmd radius
    TEllipse *te6=new TEllipse(0.,0., 75., 75.);
    te6->SetFillStyle(0); te6->SetLineColor(kBlue); te6->Draw("l"); //inner esmd radius
    
    for(int iphi=0; iphi<12; iphi++){
      float phi=(15.+ iphi*30.)/180.*TMath::Pi();
      TVector3 r; r.SetPtThetaPhi(215,0,phi);
      TVector3 rIn; rIn.SetPtThetaPhi(75,0,phi);
      TLine *tl=new TLine(rIn.X(),rIn.Y(),r.X(),r.Y());
      tl->SetLineColor(kBlue); tl->Draw(); //sector boundaries
    }
    
    //........... text information .............
    pvt = new TPaveText(0,0.,1,1,"br");
    TH1F* hText = new TH1F("text"," ",1,0,1);
    cLD->cd();
    sprintf(txt,"run=%d  eveID=%05d daq=%d vertex:ID=%d Z=%.0fcm ",runNo,eveID,daqSeq,myV.id,myV.z);
    printf("WeventDisplay::Event ID  %s\n",txt);
    pvt->AddText(txt); hText->SetTitle(Form("%s%s",hText->GetTitle(),txt));
    sprintf(txt,"TPC PT(GeV/c) prim=%.1f  near=%.1f  away=%.1f ", myTr.prMuTrack->pt(),myTr.nearTpcPT, myTr.awayTpcPT);
    printf("WeventDisplay::Event %s\n",txt);
    pvt->AddText(txt); hText->SetTitle(Form("%s%s",hText->GetTitle(),txt));
    
    sprintf(txt,"ETOW ET/GeV: 2x2=%.1f  EMC: near= %.1f   away= %.1f ",myTr.cluster.ET,myTr.nearEmcET,myTr.awayEmcET);
    printf("WeventDisplay:: %s\n",txt);
    pvt->AddText(txt); hText->SetTitle(Form("%s%s",hText->GetTitle(),txt));
    
    sprintf(txt,"total ET/GeV:   near= %.1f   away= %.1f  ptBalance= %.1f ",myTr.nearTotET,myTr.awayTotET,myTr.ptBalance.Perp());
    printf("WeventDisplay:: %s\n",txt);
    pvt->AddText(txt); hText->SetTitle(Form("%s%s",hText->GetTitle(),txt));
    
    sprintf(txt,"Q/Pt = %.3f   : ESMD E/MeV  U peak= %.1f  V peak= %.1f ",(1.0*myTr.prMuTrack->charge())/myTr.prMuTrack->pt(),myTr.esmdPeakSumE[0],myTr.esmdPeakSumE[1]);
    printf("WeventDisplay:: %s\n",txt);
    pvt->AddText(txt); hText->SetTitle(Form("%s%s",hText->GetTitle(),txt));

    float chi2=myTr.glMuTrack->chi2(); if(chi2>999.) chi2=-1.;
    sprintf(txt,"Track: eta=%.1f Q=%d nFit=%d nPoss=%d r1=%.0f r2=%.0f chi2=%.1f",myTr.pointTower.R.Eta(),myTr.prMuTrack->charge(),myTr.prMuTrack->nHitsFit(),myTr.prMuTrack->nHitsPoss(),myTr.glMuTrack->firstPoint().perp(),myTr.glMuTrack->lastPoint().perp(),chi2);
    printf("WeventDisplay:: %s\n",txt);
    pvt->AddText(txt); hText->SetTitle(Form("%s%s",hText->GetTitle(),txt));

    // save dump of histos
    if(hf.IsOpen()) {
      hText->Write();
      hEmcET->Write();
      hTpcET->Write();
      hEsmdShower[0]->Write();
      hEsmdShower[1]->Write();
      hEsmdXpt->Write();
      hf.Close();
    }
  }
  
  pvt->Draw();
  c0->Print();
    
}

//-----------------------------
//-----------------------------
void
WeventDisplay::exportEvent( const char *tit, WeveVertex myV, WeveEleTrack myTr, int vertexIndex){
  if(maxEve<=0) return;
  clear();
  int eveId=wMK->wEve->id; //wMK->mMuDstMaker->muDst()->event()->eventId();
  int runNo=wMK->wEve->runNo; //wMK->mMuDstMaker->muDst()->event()->runId();
  const char *afile = ""; //wMK->mMuDstMaker->GetFile();
  int len=strlen(afile);
  int daqSeq=atoi(afile+(len-18));
  //  printf("DDD %s len=%d %d =%s=\n",afile,len,daqSeq,afile+(len-15));

  TVector3 rTw=myTr.cluster.position;
  rTw.SetZ(rTw.z()-myV.z);
  //printf("#xcheck-%s run=%d daqSeq=%d eveID=%7d vertID=%2d zVert=%.1f prTrID=%4d  prTrEta=%.3f prTrPhi/deg=%.1f globPT=%.1f hitTwId=%4d twAdc=%.1f clEta=%.3f clPhi/deg=%.1f  clET=%.1f\n",tit,
  //	 runNo,daqSeq,eveId,myV.id,myV.z,
  //	 myTr.prMuTrack->id(),myTr.prMuTrack->eta(),myTr.prMuTrack->phi()/3.1416*180.,myTr.glMuTrack->pt(),
  //	 myTr.pointTower.id,wMK->wEve->bemc.adcTile[kBTow][myTr.pointTower.id-1],
  //	 rTw.Eta(),rTw.Phi()/3.1416*180.,myTr.cluster.ET);

  float zVert=myV.z;
  printf("WeventDisplay-%s::export run=%d eve=%d\n",tit,runNo,eveId);

 //.... process BTOW hits
  for(int i=0;i< mxBtow;i++) {
    float ene=wMK->wEve->bemc.eneTile[kBTow][i];
    if(ene<=0) continue;
    TVector3 primP=wMK->positionBtow[i]-TVector3(0,0,zVert);
    primP.SetMag(ene); // it is 3D momentum in the event ref frame
    float ET=primP.Perp();

    float eveEta=primP.Eta();
    float evePhi=primP.Phi();
    hEmcET->Fill(eveEta,evePhi,ET); 
  }
  
  //.... store ETOW hits    
  for(int i=0;i<mxEtowPhiBin;i++){
    for(int j=0;j<mxEtowEta;j++){
      float ene=wMK->wEve->etow.ene[i][j];
      if(ene<=0) continue;
      TVector3 primP=wMK->positionEtow[i][j]-TVector3(0,0,zVert);
      primP.SetMag(ene); // it is 3D momentum in the event ref frame
      float ET=primP.Perp();
      
      float eveEta=primP.Eta();
      float evePhi=primP.Phi();
      hEmcET->Fill(eveEta,evePhi,ET);	
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
    
  // compute approximate event eta for Endcap
  rL=TVector3(0,214,270-myV.z); // detector eta~1.06
  rR=TVector3(0,77,270-myV.z); // detector eta 2.0
  etaL=rL.Eta(); etaR=rR.Eta();
  etaEL_ln->SetX1(etaL);  etaEL_ln->SetX2(etaL);
  etaER_ln->SetX1(etaR);  etaER_ln->SetX2(etaR);


  //... TPC
  hTpcET->SetMinimum(0.3);hTpcET->SetMaximum(10.);
  if(wMK->mMuDstMaker) getPrimTracks( myV.id,myTr.pointTower.id);
  else getPrimTracksFromTree(vertexIndex,myTr.pointTower.id);
 
  //.... BSMD-Eta, -Phi
  
  for(int iep=0;iep<mxBSmd;iep++) {
    hBsmdAdc[iep]->SetMinimum(30);hBsmdAdc[iep]->SetMaximum(999);
    for(int i=0;i< mxBStrips;i++) {
      float adc=wMK->wEve->bemc.adcBsmd[iep][i];
      if(adc<=0) continue;
      TVector3 r=wMK->positionBsmd[iep][i];
      float z1=r.z()-zVert;
      r.SetZ(z1);
      hBsmdAdc[iep]->Fill(r.Eta(),r.Phi(),adc);
    }
  }// end of eta,phi-planes

  //.... ESMD shower shape
  for(int iuv=0; iuv<mxEsmdPlane; iuv++)
    for(int j=0;j<41;j++)
      hEsmdShower[iuv]->SetBinContent(j+1,myTr.esmdShower[iuv][j]);

  //...  ESMD cluster picture
  //initialize histo centred at track extrapolation
  TVector3 rW=myTr.pointTower.R; //z is at SMD depth
  float width=65.;
  hEsmdXpt->SetBins(130,rW.X()-width,rW.X()+width,130,rW.Y()-width,rW.Y()+width);

  //.... produce plot & save
  draw(tit,eveId, daqSeq,runNo,myV, myTr);
  //export2sketchup(tit,myV, myTr);
}

//-----------------------------
//-----------------------------
void
WeventDisplay::getPrimTracks( int vertID,int pointTowId) {
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
    if(prTr->flag()!=301 && pointTowId>0) continue;// TPC-only regular tracks for barrel candidate
    if(prTr->flag()!=301 && prTr->flag()!=311 && pointTowId<0) continue;// TPC regular and short EEMC tracks for endcap candidate
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
WeventDisplay::getPrimTracksFromTree(int vertID,int pointTowId) {

  // flag=2 use 2D cut, 1= only delta phi

  assert(vertID>=0);
  assert(vertID<(int)wMK->wEve->vertex.size());

  WeveVertex &V=wMK->wEve->vertex[vertID];
  for(uint it=0;it<V.prTrList.size();it++){
    StMuTrack *prTr=V.prTrList[it];
    if(prTr->flag()<=0) continue;
    if(prTr->flag()!=301 && pointTowId>0) continue;// TPC-only regular tracks for barrel candidate
    if(prTr->flag()!=301 && prTr->flag()!=311 && pointTowId<0) continue;// TPC regular and short EEMC tracks for endcap candidate
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
    float ene=wMK->wEve->bemc.eneTile[kBTow][i];
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
      float adc=wMK->wEve->bemc.adcBsmd[iep][i];
      if(adc<=0) continue;
      TVector3 r=wMK->positionBsmd[iep][i];
      fprintf(fd,"bsmd%c V %.1f %.3f %.3f  adc:detEta:detPhi %.3f %.3f  %.3f\n",cPlane[iep],rV.x(),rV.y(),rV.z(),adc,r.Eta(),r.Phi() );
    }
  }

  //........DUMP ETOW towers
  for(int iphi=0; iphi<mxEtowPhiBin; iphi++){
    for(int ieta=0; ieta<mxEtowEta; ieta++){//sum all eta rings
      float ene=wMK->wEve->etow.ene[iphi][ieta];
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
// Revision 1.6  2012/09/21 16:59:10  balewski
// added ESMD peak adjustement - partialy finished
//
// Revision 1.5  2012/09/18 21:10:09  stevens4
// Include all rank>0 vertex again (new jet format coming next), and remove rank<0 endcap vertices.
//
// Revision 1.4  2012/08/21 18:29:16  stevens4
// Updates to endcap W selection using ESMD strip ratio
//
// Revision 1.3  2012/06/25 20:53:29  stevens4
// algo and histo cleanup
//
// Revision 1.2  2012/06/18 18:28:01  stevens4
// Updates for Run 9+11+12 AL analysis
//
// Revision 1.1  2011/02/10 20:33:26  balewski
// start
//
