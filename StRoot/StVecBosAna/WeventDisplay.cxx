#include <stdio.h>

#include "TMath.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TFile.h"
#include "TLatex.h"
#include "TList.h"
#include "TBox.h"
#include "TPaveText.h"
#include "TPad.h"
#include "TEllipse.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"

#include "StVecBosMaker.h"
#include "Globals.h"
#include "WeventDisplay.h"


WeventDisplay::WeventDisplay(StVecBosMaker *mk, int mxEv)
{
   maxEve = mxEv;
   wMK    = mk;
   const float PI = TMath::Pi();
   const char cPlane[mxBSmd] = {'E', 'P'};
   const char cEsmdPlane[mxEsmdPlane] = {'U', 'V'};
   char txt1[100], txt2[1000];

   // barrel
   etaBL_ln = new TLine(-1, -3.2, 1, 3.2); etaBL_ln->SetLineColor(kBlue);
   etaBR_ln = new TLine(-1, -3.2, 1, 3.2); etaBR_ln->SetLineColor(kBlue);

   etaEL_ln = new TLine(2, -3.2, 3, 3.2); etaEL_ln->SetLineColor(kGreen);
   etaER_ln = new TLine(2, -3.2, 3, 3.2); etaER_ln->SetLineColor(kGreen);

   bxT = new TBox(-1.3, 0, 1.3, 1.);  bxT->SetFillStyle(0); bxT->SetLineStyle(2);
   bxE = new TBox(-1.3, 0, 1, 2.);  bxE->SetFillStyle(0); bxE->SetLineStyle(2);
   bxE->SetX2(2.2);

   hEmcET = new TH2F("eveBtowET", "EMC ET sum, Z=[0.3,30]GeV; event eta ; phi", 38, -1.4, 2.4, 63, -PI, PI);
   hTpcET = new TH2F("eveTpcET",  "TPC PT sum, Z[0.3,10]GeV/c; event eta ; phi", 32, -1.6, 1.6, 63, -PI, PI);

   for (int iep = 0; iep < mxBSmd; iep++) {
      sprintf(txt1, "eveBsmdAdc_%c", cPlane[iep]);
      sprintf(txt2, "BSMD_%c  ADC sum Z=[30,1000]; event eta ; phi", cPlane[iep]);
      hBsmdAdc[iep] = new TH2F(txt1, txt2, 26, -1.3, 1.3, 63, -PI, PI);
   }

   // ESMD shower shape
   for (int iuv = 0; iuv < mxEsmdPlane; iuv++) {
      sprintf(txt1, "eveEsmdShower_%c", cEsmdPlane[iuv]);
      sprintf(txt2, "ESMD_%c Shower Shape; i_strip position - track position (cm) ; MeV", cEsmdPlane[iuv]);
      hEsmdShower[iuv] = new TH1F(txt1, txt2, 41, -10.25, 10.25);
   }

   hEsmdXpt = new TH2F("eveEsmdXpt", "ESMD Cross Point; X (cm); Y (cm)", 100, 0., 100, 100, 0., 100.);

#if 0
   //---- smart code from Willie
   float etabinsA[1 + mxBetaStrMod * 2], etaphibinsA[mxBMod2Pi + 1];
   for (int i = 0; i < mxBetaStrMod + 1; i++)
      etabinsA[mxBetaStrMod - i] = -(etabinsA[mxBetaStrMod + i] = wMK->mSmdEGeom->EtaB()[i]);
   for (int i = 0; i < mxBMod2Pi + 1; i++)
      etaphibinsA[i] = wMK->mSmdEGeom->PhiB()[i];
   hBsmdEtaAdc = new TH2F("eveBsmdEtaAdc", " Event: BSMD-Eta ADC vs. eta & phi; pseudorapidity; azimuth", mxBetaStrMod * 2, etabinsA, mxBMod2Pi, etaphibinsA);
#endif
}


void WeventDisplay::clear()
{
   hEmcET->Reset();
   hTpcET->Reset();
   for (int iep = 0; iep < mxBSmd; iep++)       hBsmdAdc[iep]->Reset();
   for (int iuv = 0; iuv < mxEsmdPlane; iuv++)  hEsmdShower[iuv]->Reset();
   hEsmdXpt->Reset();
}


void WeventDisplay::draw(const char *tit, int eveID, int daqSeq, int runNo,
   VecBosVertex &myV, VecBosTrack &myTr)
{
   if (maxEve <= 0) return;
   maxEve--;
   TStyle *myStyle = new TStyle();
   myStyle->cd();
   myStyle->SetPalette(1, 0);
   myStyle->SetOptStat(1000010);

   char txt[1000];
   sprintf(txt, "display-%s_run%d.eventId%06dvert%d", tit, runNo, eveID, myV.mId);

   printf("WeventDisplay::Draw %s\n", txt);
   TCanvas *c0; TPaveText *pvt;
   string detector = tit;

   if (detector.compare("WB") == 0) { // barrel event display
      sprintf(txt, "display-%s%.0f_run%d.eventId%05dvert%d", tit, myTr.mCluster2x2.ET, runNo, eveID, myV.mId);
      TFile hf(Form("%s.root", txt), "recreate"); //TFile hf(txt,"recreate");
      c0 = new TCanvas(txt, txt, 850, 600);
      c0->cd();

      TString tt = txt;

      TPad *cU = new TPad(tt + "U", tt + "U", 0., 0.2, 1., 1.); cU->Draw();
      TPad *cD = new TPad(tt + "D", tt + "D", 0., 0., 1., 0.2); cD->Draw();
      cU->cd();

      TPad *cU1 = new TPad(tt + "U1", tt + "U1", 0., 0., 0.24, 1.); cU1->Draw();
      TPad *cU2 = new TPad(tt + "U2", tt + "U2", 0.24, 0., 0.55, 1.); cU2->Draw();
      TPad *cU3 = new TPad(tt + "U3", tt + "U3", 0.55, 0., 1., 1.);  cU3->Draw();
      cU3->Divide(2, 1);
      cU1->cd();
      hTpcET->Draw("colz");

      TVector3 rW = myTr.mMatchedTower.R;
      rW.SetZ(rW.z() - myV.z);

      TEllipse *te1 = new TEllipse(rW.Eta(), rW.Phi(), 0.1, 0.1);
      te1->SetFillStyle(0); te1->SetLineStyle(3); te1->SetLineColor(kMagenta);
      TEllipse *te2 = new TEllipse(rW.Eta(), rW.Phi(), 0.7, 0.7);
      te2->SetFillStyle(0); te2->SetLineStyle(3); te2->SetLineColor(kBlack);

      TVector3 rA = -rW; // away direction
      bxT->SetY1(rA.Phi() - VecBosEvent::sMinTrackIsoDeltaPhi);
      bxT->SetY2(rA.Phi() + VecBosEvent::sMinTrackIsoDeltaPhi);

      bxE->SetY1(rA.Phi() - VecBosEvent::sMinTrackIsoDeltaPhi);
      bxE->SetY2(rA.Phi() + VecBosEvent::sMinTrackIsoDeltaPhi);

      te1->Draw(); te2->Draw(); bxT->Draw("l");
      etaBL_ln->Draw(); etaBR_ln->Draw(); etaEL_ln->Draw();

      cU2->cd();    hEmcET->Draw("colz");
      te1->Draw();  te2->Draw(); bxE->Draw("l");
      etaBL_ln->Draw();  etaBR_ln->Draw();
      etaEL_ln->Draw();  etaER_ln->Draw();

      for (int iep = 0; iep < mxBSmd; iep++)
      {
         cU3->cd(1 + iep);
         hBsmdAdc[iep]->Draw("colz");
         te1->Draw();
         te2->Draw();
         bxT->Draw("l");
         etaBL_ln->Draw();
         etaBR_ln->Draw();
      }

      // text information
      pvt = new TPaveText(0, 0., 1, 1, "br");
      cD->cd();
      sprintf(txt, "run=%d  eveID=%05d daq=%d vertex:mId=%d Z=%.0fcm", runNo, eveID, daqSeq, myV.mId, myV.z);
      printf("WeventDisplay::Event ID  %s\n", txt);
      pvt->AddText(txt);

      sprintf(txt, "TPC PT(GeV/c) near=%.1f  away=%.1f ", myTr.mP3InNearConeTpc.Pt(), myTr.awayTpcPT);
      printf("WeventDisplay::Event TPC  %s\n", txt);
      pvt->AddText(txt);

      sprintf(txt, "BTOW ET/GeV: 2x2=%.1f   near= %.1f   away= %.1f", myTr.mCluster2x2.ET, myTr.mP3InNearConeBTow.Pt(), myTr.awayBtowET);
      printf("WeventDisplay:: BTOW  %s\n", txt);
      pvt->AddText(txt);

      sprintf(txt, "Emc (Btow+Etow) ET/GeV:   near= %.1f   away= %.1f", myTr.mP3InNearCone.Pt(), myTr.awayEmcET);
      printf("WeventDisplay:: BTOW+ETOW  %s\n", txt);
      pvt->AddText(txt);

      sprintf(txt, "total ET/GeV:   near= %.1f   away= %.1f  ptBalance= %.1f", myTr.mP3InNearCone.Pt(), myTr.awayTotET, myTr.ptBalance.Perp());
      printf("WeventDisplay:: BTOW  %s\n", txt);
      pvt->AddText(txt);
      // save dump of histos
      if (hf.IsOpen()) {
         hEmcET->Write();
         hTpcET->Write();
         for (int iep = 0; iep < mxBSmd; iep++) hBsmdAdc[iep]->Write();
         hf.Close();
      }
   }
   else if (detector.compare("WE") == 0) { // endcap event display
      sprintf(txt, "display-%s%.0f_run%d.eventId%05dvert%d", tit, myTr.mCluster2x2.ET, runNo, eveID, myV.mId);
      TFile hf(Form("%s.root", txt), "recreate");
      c0 = new TCanvas(txt, txt, 1750, 1300);
      c0->cd();

      TString tt = txt;
      TPad *cL = new TPad(tt + "L", tt + "L", 0., 0., 0.6, 1.); cL->Draw();
      TPad *cR = new TPad(tt + "R", tt + "R", 0.6, 0., 1., 1.); cR->Draw();
      cL->cd();
      TPad *cLU = new TPad(tt + "LU", tt + "LU", 0., 0.2, 1., 1.); cLU->Draw();
      TPad *cLD = new TPad(tt + "LD", tt + "LD", 0., 0., 1., 0.2); cLD->Draw();
      cLU->cd();
      TPad *cLU1 = new TPad(tt + "LU1", tt + "LU1", 0., 0., 0.44, 1.); cLU1->Draw();
      TPad *cLU2 = new TPad(tt + "LU2", tt + "LU2", 0.44, 0., 1., 1.); cLU2->Draw();
      cR->cd();
      TPad *cRU = new TPad(tt + "RU", tt + "RU", 0., 0.5, 1., 1.); cRU->Draw();
      TPad *cRD = new TPad(tt + "RD", tt + "RD", 0., 0., 1., 0.5); cRD->Draw();
      cRD->Divide(1, 2);
      cLU1->cd(); hTpcET->Draw("colz");

      TVector3 rW = myTr.mMatchedTower.R;
      rW.SetZ(rW.z() - myV.z);
      TEllipse *te1 = new TEllipse(rW.Eta(), rW.Phi(), 0.1, 0.1);
      te1->SetFillStyle(0); te1->SetLineStyle(3); te1->SetLineColor(kMagenta);
      TEllipse *te2 = new TEllipse(rW.Eta(), rW.Phi(), 0.7, 0.7);
      te2->SetFillStyle(0); te2->SetLineStyle(3); te2->SetLineColor(kBlack);

      TVector3 rA = -rW; // away direction
      bxT->SetY1(rA.Phi() - VecBosEvent::sMinTrackIsoDeltaPhi);
      bxT->SetY2(rA.Phi() + VecBosEvent::sMinTrackIsoDeltaPhi);

      bxE->SetY1(rA.Phi() - VecBosEvent::sMinTrackIsoDeltaPhi);
      bxE->SetY2(rA.Phi() + VecBosEvent::sMinTrackIsoDeltaPhi);

      te1->Draw();   te2->Draw(); bxT->Draw("l");
      etaBL_ln->Draw();  etaBR_ln->Draw(); etaEL_ln->Draw();

      cLU2->cd(); 
      hEmcET->Draw("colz");
      te1->Draw(); te2->Draw(); bxE->Draw("l");
      etaBL_ln->Draw();  etaBR_ln->Draw();
      etaEL_ln->Draw();  etaER_ln->Draw();

      TList *Lx;  TLine *tline; TLine *tlineGlob;
      //Draw ESMD showers
      for (int iuv = 0; iuv < mxEsmdPlane; iuv++) {
         cRD->cd(1 + iuv);

         Lx = hEsmdShower[iuv]->GetListOfFunctions();
         tline = new TLine(myTr.esmdDca[iuv], 0., myTr.esmdDca[iuv], 100.);
         tline->SetLineColor(2);
         tlineGlob = new TLine(myTr.esmdGlobStrip[iuv] * 0.5 + myTr.esmdDcaGlob[iuv], 0, myTr.esmdGlobStrip[iuv] * 0.5 + myTr.esmdDcaGlob[iuv], 100.); tlineGlob->SetLineColor(2);
         hEsmdShower[iuv]->Draw();
         tline->Draw(); Lx->Add(tline);
         tlineGlob->SetLineStyle(2); tlineGlob->Draw(); Lx->Add(tlineGlob);
         hEsmdShower[iuv]->Draw();

         //print Q/Pt warning
         if ( iuv == 1 && myTr.mStMuTrack->pt() >= 100.0 ) {
            TLatex *tx = new TLatex(3, 0.9 * hEsmdShower[iuv]->GetMaximum(), "| Q/P_{T} | < 0.01");  tx->SetTextColor(kRed); tx->SetTextSize(0.1); tx->Draw();
         }
      }
      hEsmdShower[0]->SetFillColor(kBlue - 5);
      hEsmdShower[1]->SetFillColor(kGreen - 5);

      //make SMD cluster picture
      cRU->cd();
      hEsmdXpt->Draw("colz");
      //fill with lines of hit strips
      int secLoop[3] = {myTr.hitSector - 2, myTr.hitSector - 1, myTr.hitSector};
      if (secLoop[0] < 0) secLoop[0] = 11;
      if (secLoop[2] > 11) secLoop[2] = 0;

      for (int iuv = 0; iuv < mxEsmdPlane; iuv++) {
         for (int isec = 0; isec < 3; isec++) {
            for (int k = 0; k < 288; k++) { //loop all strips
               const StructEEmcStrip *stripPtr = wMK->mGeomSmd->getStripPtr(k, iuv, secLoop[isec]); //myTr.esmdStripId[iuv][k]
               if (!stripPtr) continue;
               if (wMK->mVecBosEvent->esmd.ene[secLoop[isec]][iuv][k] * 1e3 < 10.) continue;
               int nColor = (int) (wMK->mVecBosEvent->esmd.ene[secLoop[isec]][iuv][k] * 1e3) / 20;
               int nSub = -10;
               if (nColor == 0) nSub = 10;
               if (nColor == 1) nSub = 8;
               if (nColor == 2) nSub = 5;
               if (nColor == 3) nSub = 1;
               if (nColor == 4) nSub = -4;
               TVector3 end1 = stripPtr->end1;
               TVector3 end2 = stripPtr->end2;
               Lx = hEsmdXpt->GetListOfFunctions();
               tline = new TLine(end1.X(), end1.Y(), end2.X(), end2.Y());
               if (iuv == 0) tline->SetLineColor(kBlue - nSub);
               if (iuv == 1) tline->SetLineColor(kGreen - nSub);
               if (wMK->mVecBosEvent->esmd.ene[secLoop[isec]][iuv][k] * 1e3 > 100) tline->SetLineColor(2);
               tline->Draw(); Lx->Add(tline);
            }
         }
      }

      TEllipse *te3 = new TEllipse(rW.X(), rW.Y(), 2.0, 2.0);
      te3->SetLineColor(kBlue); te3->SetFillStyle(0); te3->Draw(); //track projection
      //TEllipse *te5=new TEllipse(myTr.mMatchedTower.Rglob.X(),myTr.mMatchedTower.Rglob.Y(), .25, .25);
      //te5->SetLineColor(kBlue); te5->Draw(); //global track projection
      TEllipse *te4 = new TEllipse(0., 0., 215., 215.);
      te4->SetFillStyle(0); te4->SetLineColor(kBlue); te4->Draw("l"); //outer esmd radius
      TEllipse *te6 = new TEllipse(0., 0., 75., 75.);
      te6->SetFillStyle(0); te6->SetLineColor(kBlue); te6->Draw("l"); //inner esmd radius

      for (int iphi = 0; iphi < 12; iphi++) {
         float phi = (15. + iphi * 30.) / 180.*TMath::Pi();
         TVector3 r; r.SetPtThetaPhi(215, 0, phi);
         TVector3 rIn; rIn.SetPtThetaPhi(75, 0, phi);
         TLine *tl = new TLine(rIn.X(), rIn.Y(), r.X(), r.Y());
         tl->SetLineColor(kBlue); tl->Draw(); //sector boundaries
      }

      // text information
      pvt = new TPaveText(0, 0., 1, 1, "br");
      TH1F *hText = new TH1F("text", " ", 1, 0, 1);
      cLD->cd();
      sprintf(txt, "run=%d  eveID=%05d daq=%d vertex:mId=%d Z=%.0fcm ", runNo, eveID, daqSeq, myV.mId, myV.z);
      printf("WeventDisplay::Event ID  %s\n", txt);
      pvt->AddText(txt); hText->SetTitle(Form("%s%s", hText->GetTitle(), txt));
      sprintf(txt, "TPC PT(GeV/c) prim=%.1f  near=%.1f  away=%.1f ", myTr.mStMuTrack->pt(), myTr.mP3InNearConeTpc.Pt(), myTr.awayTpcPT);
      printf("WeventDisplay::Event %s\n", txt);
      pvt->AddText(txt); hText->SetTitle(Form("%s%s", hText->GetTitle(), txt));

      sprintf(txt, "ETOW ET/GeV: 2x2=%.1f  EMC: near= %.1f   away= %.1f ", myTr.mCluster2x2.ET, myTr.mP3InNearCone.Pt(), myTr.awayEmcET);
      printf("WeventDisplay:: %s\n", txt);
      pvt->AddText(txt); hText->SetTitle(Form("%s%s", hText->GetTitle(), txt));

      sprintf(txt, "total ET/GeV:   near= %.1f   away= %.1f  ptBalance= %.1f ", myTr.mP3InNearCone.Pt(), myTr.awayTotET, myTr.ptBalance.Perp());
      printf("WeventDisplay:: %s\n", txt);
      pvt->AddText(txt); hText->SetTitle(Form("%s%s", hText->GetTitle(), txt));

      sprintf(txt, "Q/Pt = %.3f   : ESMD E/MeV  U plane= %.1f  V plane= %.1f ", (1.0 * myTr.mStMuTrack->charge()) / myTr.mStMuTrack->pt(), myTr.esmdE[0], myTr.esmdE[1]);
      printf("WeventDisplay:: %s\n", txt);
      pvt->AddText(txt); hText->SetTitle(Form("%s%s", hText->GetTitle(), txt));

      float chi2 = myTr.glMuTrack->chi2(); if (chi2 > 999.) chi2 = -1.;
      sprintf(txt, "Track: eta=%.1f Q=%d nFit=%d nPoss=%d r1=%.0f r2=%.0f chi2=%.1f", myTr.mMatchedTower.R.Eta(), myTr.mStMuTrack->charge(), myTr.mStMuTrack->nHitsFit(), myTr.mStMuTrack->nHitsPoss(), myTr.glMuTrack->firstPoint().perp(), myTr.glMuTrack->lastPoint().perp(), chi2);
      printf("WeventDisplay:: %s\n", txt);
      pvt->AddText(txt); hText->SetTitle(Form("%s%s", hText->GetTitle(), txt));

      // save dump of histos
      if (hf.IsOpen()) {
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


void WeventDisplay::exportEvent(const char *detType, VecBosVertex &myV,
   VecBosTrack &myTr, int vertexIndex)
{
   if (maxEve <= 0) return;
   clear();

   int eveId = wMK->mVecBosEvent->GetEventId();
   int runNo = wMK->mVecBosEvent->GetRunId();

   const char *afile = ""; //wMK->mStMuDstMaker->GetFile();
   int len    = strlen(afile);
   int daqSeq = atoi(afile + (len - 18));
   //printf("DDD %s len=%d %d =%s=\n",afile,len,daqSeq,afile+(len-15));

   TVector3 rTw = myTr.mCluster2x2.position;
   rTw.SetZ(rTw.z() - myV.z);

   //printf("#xcheck-%s run=%d daqSeq=%d eveID=%7d vertID=%2d zVert=%.1f prTrID=%4d  prTrEta=%.3f prTrPhi/deg=%.1f globPT=%.1f hitTwId=%4d twAdc=%.1f clEta=%.3f clPhi/deg=%.1f  clET=%.1f\n",detType,
   //	 runNo,daqSeq,eveId,myV.id,myV.z,
   //	 myTr.mStMuTrack->id(),myTr.mStMuTrack->eta(),myTr.mStMuTrack->phi()/3.1416*180.,myTr.glMuTrack->pt(),
   //	 myTr.mMatchedTower.id,wMK->mVecBosEvent->bemc.adcTile[kBTow][myTr.mMatchedTower.id-1],
   //	 rTw.Eta(),rTw.Phi()/3.1416*180.,myTr.mCluster2x2.ET);

   float zVert = myV.z;
   printf("WeventDisplay-%s::export run=%d eve=%d\n", detType, runNo, eveId);

   // process BTOW hits
   for (int i = 0; i < mxBtow; i++) {
      float ene = wMK->mVecBosEvent->bemc.eneTile[kBTow][i];
      if (ene <= 0) continue;
      TVector3 mP3AtDca = gBCalTowerCoords[i] - TVector3(0, 0, zVert);
      mP3AtDca.SetMag(ene); // it is 3D momentum in the event ref frame
      float ET = mP3AtDca.Perp();

      float eveEta = mP3AtDca.Eta();
      float evePhi = mP3AtDca.Phi();
      hEmcET->Fill(eveEta, evePhi, ET);
   }

   // store ETOW hits
   for (int i = 0; i < mxEtowPhiBin; i++) {
      for (int j = 0; j < mxEtowEta; j++) {
         float ene = wMK->mVecBosEvent->etow.ene[i][j];
         if (ene <= 0) continue;
         TVector3 mP3AtDca = gETowCoords[i][j] - TVector3(0, 0, zVert);
         mP3AtDca.SetMag(ene); // it is 3D momentum in the event ref frame
         float ET = mP3AtDca.Perp();

         float eveEta = mP3AtDca.Eta();
         float evePhi = mP3AtDca.Phi();
         hEmcET->Fill(eveEta, evePhi, ET);
      }
   }

   hEmcET->SetMinimum(0.3);
   hEmcET->SetMaximum(30.);

   // compute approximate event eta for barrel
   float x, y, z;
   float Rcylinder = gBTowGeom->Radius();
   assert(gBTowGeom->getXYZ(20, x, y, z) == 0); // this is approximate Z of last tower
   TVector3 rL(Rcylinder, 0, z + myV.z);
   TVector3 rR(Rcylinder, 0, z - myV.z);
   float etaL = -rL.Eta(), etaR = rR.Eta();
   etaBL_ln->SetX1(etaL);  etaBL_ln->SetX2(etaL);
   etaBR_ln->SetX1(etaR);  etaBR_ln->SetX2(etaR);

   // compute approximate event eta for Endcap
   rL = TVector3(0, 214, 270 - myV.z); // detector eta~1.06
   rR = TVector3(0, 77, 270 - myV.z); // detector eta 2.0
   etaL = rL.Eta(); etaR = rR.Eta();
   etaEL_ln->SetX1(etaL);  etaEL_ln->SetX2(etaL);
   etaER_ln->SetX1(etaR);  etaER_ln->SetX2(etaR);


   // TPC
   hTpcET->SetMinimum(0.3);
   hTpcET->SetMaximum(10.);

   if (wMK->mStMuDstMaker)
      getPrimTracks( myV.mId, myTr.mMatchedTower.id);
   //XXX:ds else
   //   getPrimTracksFromTree(vertexIndex, myTr.mMatchedTower.id);

   // BSMD-Eta, -Phi
   for (int iep = 0; iep < mxBSmd; iep++)
   {
      hBsmdAdc[iep]->SetMinimum(30);
      hBsmdAdc[iep]->SetMaximum(999);
      for (int i = 0; i < mxBStrips; i++) {
         float adc = wMK->mVecBosEvent->bemc.adcBsmd[iep][i];
         if (adc <= 0) continue;
         TVector3 r = gBSmdStripCoords[iep][i];
         float z1 = r.z() - zVert;
         r.SetZ(z1);
         hBsmdAdc[iep]->Fill(r.Eta(), r.Phi(), adc);
      }
   }

   // ESMD shower shape
   for (int iuv = 0; iuv < mxEsmdPlane; iuv++)
      for (int j = 0; j < 41; j++)
         hEsmdShower[iuv]->SetBinContent(j + 1, myTr.esmdShower[iuv][j]);

   // ESMD cluster picture
   //initialize histo centred at track extrapolation
   TVector3 rW = myTr.mMatchedTower.R; // z is at SMD depth
   float width = 65.;
   hEsmdXpt->SetBins(130, rW.X() - width, rW.X() + width, 130, rW.Y() - width, rW.Y() + width);

   // produce plot & save
   draw(detType, eveId, daqSeq, runNo, myV, myTr);
   //export2sketchup(detType,myV, myTr);
}


void WeventDisplay::getPrimTracks( int vertID, int pointTowId)
{
   assert(vertID >= 0);
   assert(vertID < (int)wMK->mStMuDstMaker->muDst()->numberOfPrimaryVertices());
   StMuPrimaryVertex *V = wMK-> mStMuDstMaker->muDst()->primaryVertex(vertID);
   assert(V);
   wMK-> mStMuDstMaker->muDst()->setVertexIndex(vertID);
   float rank = V->ranking();
   assert(rank > 0 || (rank < 0 && V->nEEMCMatch()));

   Int_t nPrimTrAll = wMK->mStMuDstMaker->muDst()->GetNPrimaryTrack();
   for (int itr = 0; itr < nPrimTrAll; itr++) {
      StMuTrack *prTr = wMK->mStMuDstMaker->muDst()->primaryTracks(itr);
      if (prTr->flag() <= 0) continue;
      if (prTr->flag() != 301 && pointTowId > 0) continue; // TPC-only regular tracks for barrel candidate
      if (prTr->flag() != 301 && prTr->flag() != 311 && pointTowId < 0) continue; // TPC regular and short EEMC tracks for endcap candidate
      float hitFrac = 1.*prTr->nHitsFit() / prTr->nHitsPoss();
      if (hitFrac < VecBosEvent::sMinTrackHitFrac) continue;
      StThreeVectorF prPvect = prTr->p();
      TVector3 mP3AtDca = TVector3(prPvect.x(), prPvect.y(), prPvect.z());
      float pT = prTr->pt();
      hTpcET->Fill(prTr->eta(), prTr->phi(), pT);

   }
}


void WeventDisplay::getPrimTracksFromTree(int vertID, int pointTowId)
{
/*
   // flag=2 use 2D cut, 1= only delta phi

   assert(vertID >= 0);
   assert(vertID < (int) wMK->mVecBosEvent->mVertices.size());

   VecBosVertex &V = wMK->mVecBosEvent->mVertices[vertID];

   for (uint it = 0; it < V.prTrList.size(); it++) {
      StMuTrack *prTr = V.prTrList[it];
      if (prTr->flag() <= 0) continue;
      if (prTr->flag() != 301 && pointTowId > 0) continue; // TPC-only regular tracks for barrel candidate
      if (prTr->flag() != 301 && prTr->flag() != 311 && pointTowId < 0) continue; // TPC regular and short EEMC tracks for endcap candidate
      float hitFrac = 1.*prTr->nHitsFit() / prTr->nHitsPoss();
      if (hitFrac < VecBosEvent::sMinTrackHitFrac) continue;
      StThreeVectorF prPvect = prTr->p();
      TVector3 mP3AtDca = TVector3(prPvect.x(), prPvect.y(), prPvect.z());
      float pT = prTr->pt();
      hTpcET->Fill(prTr->eta(), prTr->phi(), pT);

   }
*/
}


void WeventDisplay::export2sketchup(const char *tit, VecBosVertex &myV, VecBosTrack &myTr)
{
   int eveId = wMK->mStMuDstMaker->muDst()->event()->eventId();
   int runNo = wMK->mStMuDstMaker->muDst()->event()->runId();
   char txt[1000];
   sprintf(txt, "display3D-%s_run%d.eventId%05d_vert%d.txt", tit, runNo, eveId, myV.mId);
   FILE *fd = fopen(txt, "w");
   assert(fd);

   // DUMP PRIM TRACKS
   int vertID = myV.mId;
   assert(vertID >= 0);
   assert(vertID < (int)wMK->mStMuDstMaker->muDst()->numberOfPrimaryVertices());
   StMuPrimaryVertex *V = wMK-> mStMuDstMaker->muDst()->primaryVertex(vertID);
   assert(V);
   wMK-> mStMuDstMaker->muDst()->setVertexIndex(vertID);
   float rank = V->ranking();
   assert(rank > 0 || (rank < 0 && V->nEEMCMatch()));
   const StThreeVectorF &rV = V->position();
   Int_t nPrimTrAll = wMK->mStMuDstMaker->muDst()->GetNPrimaryTrack();

   for (int itr = 0; itr < nPrimTrAll; itr++)
   {
      StMuTrack *prTr = wMK->mStMuDstMaker->muDst()->primaryTracks(itr);
      if (prTr->flag() <= 0) continue;
      if (prTr->flag() != 301) continue; // TPC-only regular tracks
      float hitFrac = 1.*prTr->nHitsFit() / prTr->nHitsPoss();
      if (hitFrac < VecBosEvent::sMinTrackHitFrac) continue;
      prTr->p();
      fprintf(fd, "track V %.1f %.3f %.3f  mP3AtDca:PT:eta:phi:Q %.1f %.3f  %.3f  %d\n", rV.x(), rV.y(), rV.z(), prTr->p().perp(), prTr->p().pseudoRapidity(), prTr->p().phi(), prTr->charge());
   }

   // Dump BTOW towers
   float Rcylinder = gBTowGeom->Radius(), Rcylinder2 = Rcylinder * Rcylinder;
   for (int i = 0; i < mxBtow; i++)
   {
      float ene = wMK->mVecBosEvent->bemc.eneTile[kBTow][i];
      if (ene <= 0) continue;
      float delZ = gBCalTowerCoords[i].z() - myV.z;
      float e2et = Rcylinder / sqrt(Rcylinder2 + delZ * delZ);
      float ET = ene * e2et;
      float detEta = gBCalTowerCoords[i].Eta();
      float detPhi = gBCalTowerCoords[i].Phi();
      fprintf(fd, "btow V %.1f %.3f %.3f  eveET:detEta:detPhi %.3f %.3f  %.3f\n", rV.x(), rV.y(), rV.z(), ET, detEta, detPhi);
   }

   const char cPlane[ mxBSmd] = {'E', 'P'};
   // Dump BSMD  hits
   for (int iep = 0; iep < mxBSmd; iep++) {
      for (int i = 0; i < mxBStrips; i++) {
         float adc = wMK->mVecBosEvent->bemc.adcBsmd[iep][i];
         if (adc <= 0) continue;
         TVector3 r = gBSmdStripCoords[iep][i];
         fprintf(fd, "bsmd%c V %.1f %.3f %.3f  adc:detEta:detPhi %.3f %.3f  %.3f\n", cPlane[iep], rV.x(), rV.y(), rV.z(), adc, r.Eta(), r.Phi() );
      }
   }

   // Dump ETOW towers
   for (int iphi = 0; iphi < mxEtowPhiBin; iphi++) {
      for (int ieta = 0; ieta < mxEtowEta; ieta++) { //sum all eta rings
         float ene = wMK->mVecBosEvent->etow.ene[iphi][ieta];
         if (ene <= 0) continue; //skip towers with no energy
         TVector3 detP = gETowCoords[iphi][ieta];
         TVector3 mP3AtDca = detP - TVector3(0, 0, myV.z);
         mP3AtDca.SetMag(ene); // it is 3D momentum in the event ref frame
         fprintf(fd, "etow V %.1f %.3f %.3f  eveET:detEta:detPhi %.3f %.3f  %.3f\n", rV.x(), rV.y(), rV.z(), mP3AtDca.Perp(), detP.Eta(), detP.Phi());
      }
   }

   // DUMP reco electron
   float eleET = myTr.mCluster2x2.ET;
   fprintf(fd, "recoElectron V %.1f %.3f %.3f  ET:detEta:detPhi %.3f %.3f  %.3f\n", rV.x(), rV.y(), rV.z() , eleET, myTr.mMatchedTower.R.Eta(), myTr.mMatchedTower.R.Phi());

   fclose(fd);
}
