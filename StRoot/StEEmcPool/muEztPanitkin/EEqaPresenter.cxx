#include <TPad.h>
#include <TH2.h>
#include <TH1.h>
#include <TF1.h>
#include <TEnv.h>
#include <TPaveStats.h>
#include <TStyle.h> // for gPad
#include <TROOT.h> // for gROOT
#include <TSystem.h>
#include <TMath.h>
#include <TObjArray.h>

#include <StMessMgr.h>

#include "EemcTwMask.h"
#include "EEqaPresenter.h"

static TObjArray *hCleanUp = new TObjArray();
TStyle *defStyle=0; 
TStyle *ee1Style=0;

//--------------------------------------
void eePlotInit() {
  defStyle=gROOT->GetStyle("Default");
  ee1Style= new TStyle("ee1STyle","ee1Style (general, minimal Stat)");
  if (ee1Style) {
    ee1Style->SetOptStat(11); 
    ee1Style->SetPalette(1);
    ee1Style->SetTextSize(0.09);
    //  ee1Style->cd();
  }
}

//--------------------------------------
TH1 *GetHisto(FileType &fd,const Char_t *name) {
    TH1 *h = (TH1*)fd.Get(name, 0);
    if (h) {
	h->SetDirectory(0);
	if (hCleanUp) hCleanUp->Add(h);
    } else {
	LOG_ERROR << "Histogram not found: " << name << endm;
    }
    return h;
}

//--------------------------------------
void eePlot(int page, int panel, FileType fd, TPad *cc, const Char_t *eemcTwMaskFilename){
  static bool first = true;
  static EemcTwMask *twMask = 0;
  if (first) {
    eePlotInit(); 
    twMask = new EemcTwMask;
    bool twMaskFound = useTwMask(eemcTwMaskFilename, twMask); 
    if (!twMaskFound) {
	delete twMask;
	twMask=0;
    }
    first = false;
  }  
  
  if (ee1Style) ee1Style->cd(); // use my default style
  if (hCleanUp) hCleanUp->Delete();
  cc->Clear();
  
  if(page==11) {
    switch(panel) {
    case 1: eeJpQa(fd, cc, twMask); break;
    case 2: eeDaqCorr(fd, cc,1); break;
      // case 3: eeFreq(fd, cc, twMask); break;
      // case 4: eeDaqTwCr(fd, cc, twMask); break;
    case 3: eeDaqTwHot(fd, cc, twMask); break;
    case 4: eeDaqTwHit(fd, cc); break;
    default:  plNone(cc); break;
    } 
  } else if (page==12) {
    switch(panel) {
    case 1: eeDaqCorr(fd, cc,2); break;
    case 2: eeDaqMapmtStat(fd, cc); break;
      // case 3: eeDaqMapmtCr(fd, cc,64); break;
      // case 4: eeDaqMapmtCr(fd, cc,72); break;
      // case 5: eeDaqMapmtCr(fd, cc,80); break;
      // case 6: eeDaqMapmtCr(fd, cc,88); break;
      // case 7: eeDaqMapmtCr(fd, cc,96); break;
      // case 8: eeDaqMapmtCr(fd, cc,104); break;
    case 3:  eeDaqSmdA(fd, cc,"SmdA",'U'); break;
    case 4: eeDaqSmdA(fd, cc,"SmdA",'V'); break;
    case 5: eeDaqSmdA(fd, cc,"HSmd",'U'); break;
    case 6: eeDaqSmdA(fd, cc,"HSmd",'V'); break;
    default:  plNone(cc); break;
      }
  } else if (page==13) {
    switch(panel) {
      // case 1: eeTrigHanks(fd, cc); break;  
    case 1: eeTrigDsm0(fd, cc,"HT"); break;
    case 2: eeTrigDsm0(fd, cc,"TP"); break;
    case 3: eeTrigDsm1(fd, cc,"HT"); break;
    case 4: eeTrigDsm1(fd, cc,"TP"); break;
    case 5: eeTrigDsm2HT(fd, cc); break;
    case 6: eeTrigJPsum(fd, cc,"_sum"); break;
    case 7: eeTrigJPfreq(fd, cc); break;
    case 8: eeTrigAdjJPsum(fd, cc,"_sum"); break;
    case 9: eeTrigAdjJPcor(fd, cc,"_cor"); break;
    case 10: eeTrigEtot(fd, cc); break;
    default:  plNone(cc); break;
    }
    }  else if (page==14) {
    switch(panel) {
    case 1: eeDaqTwCr(fd, cc, twMask); break;
    case 2: eeFreq(fd, cc, twMask); break;
    case 3: eeDaqMapmtCr(fd, cc,64); break;
    case 4: eeDaqMapmtCr(fd, cc,72); break;
    case 5: eeDaqMapmtCr(fd, cc,80); break;
    case 6: eeDaqMapmtCr(fd, cc,88); break;
    case 7: eeDaqMapmtCr(fd, cc,96); break;
    case 8: eeDaqMapmtCr(fd, cc,104); break;
    case 9: eeTrigHanks(fd, cc); break;  
    case 10:eeEmuVsSimu(fd, cc); break;

    default:  plNone(cc); break;
    }
    }

  //  if (defStyle) defStyle->cd(); // retun to default style
 LOG_DEBUG << "JB panel=" << panel << " page=" << page << " done" << endm;
 
}

//--------------------------------------
void plNone(TPad *c) {
  if (c) c->Clear();
  TText *txt = new TText(0.05,.5,"tempo disabled, Jan");
  if (txt) {
    if (hCleanUp) hCleanUp->Add(txt);
    txt->Draw();
  }
}
  
//--------------------------------------
void eeJpQa(FileType fd, TPad *c0, EemcTwMask *m) { // out
  c0->Clear();
  c0->cd(0);

  TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);  
  c->Draw();  c->cd();
  c->Divide(2,2);
  const Char_t *name1[]={"JPpedZoom","JPtotCor","JPtotFreq","JPpedHot"};
  const Char_t *name2[]={"JPpedZoom","JPsumTh3","JPtotFreq","xx"}; 
  const Char_t **name = (m == 0) ? name2 : name1; // dirty trick, JB

  TH1* H4jpHot = 0;
  for (int i = 0;i < 4;i++) {
    TH1 *h = GetHisto(fd,name[i]);
    if (!h) continue;
    c->cd(1+i);
    if(i==0) 
      h->Draw("colz");
    else
      h->Draw("b");
    if(i==0) gPad->SetLogz();
    if ((i == 1) || (i == 2)) {
	eeJpQaMinMax(h);
    }
    if (i == 3) H4jpHot = h;
  }
  
  if(m==0) return;
  if (H4jpHot) {// start counting hot towers
    H4jpHot->Reset(); // should be here, but online works w/o it 
    for(int cr=1;cr<6;cr++) {
      TH1 *hx = GetHisto(fd, Form("cr%dHot",cr));
      if (!hx) continue;
      if(hx->Integral()<=50 ) continue;
      if(hx->GetRMS()<=2.)    continue;
      hx->Fit("pol0","0Q");
      TF1 *ff=hx->GetFunction("pol0");
      float yM=ff->GetParameter(0);
      
      int nb=hx->GetNbinsX();
      int k;
      for(k=1;k<=nb;k++){
	if(hx->GetBinContent(k)<10*yM) continue;
	if(m && m->crCh[cr-1][k-1]) continue; // ignore masked channels
	LOG_DEBUG << " hot cr=" << cr << " ch=" << (k - 1) << " val=" << hx->GetBinContent(k) << endm;
	H4jpHot->Fill(cr);
      }
    }
  }
  // write text on the left 
  c0->cd(0);
  TPad *c3 = new TPad("pad3", "apd3",0.,0.,1.,.1);
  c3->Draw();
  c3->cd();
  if (m && m->txtH) m->txtH->Draw();
}

//--------------------------------------
//--------------------------------------
void eeDaqCorr(FileType fd, TPad *c, int es) { // out

  static  TPad *c2 =0;
  static  TPad *c3 =0;

  const Char_t *nameT[]={"ETowHealth","ETowHeadCorr","ETowOFF","ETowN256","ETowOFFid","ETowGhost","ETowCorrBit"};
  const Char_t *nameE[]={"ESmdHealth","ESmdHeadCorr","ESmdOFF","ESmdN256","ESmdOFFid","ESmdGhost","ESmdCorrBit"};

  const Char_t **name=nameT;
  float y1=0.3;
  int n1=6;  
  if(es==2) {
    name=nameE;
    y1=0.45;
    n1=4;
  }

  c->cd(0);
  c2 = new TPad("pad2", "apd2",0.0,y1+0.01,1.,1.);  
  c2->Draw();  c2->cd();
  c2->Divide(2,n1/2);
  
  for(int i=0;i<n1;i++) {
    TH1 *h = GetHisto(fd,name[i]);
    if(!h) continue;
    c2->cd(1+i);
    h->Draw();
    //    gPad->SetLogy(0);
    //  if(h->Integral()>0 ) gPad->SetLogy();
    // if(i<2) gPad->SetLogx();
  }

  if(es==2) {
    c->cd(0);
    float y2=y1*0.7;
    c2 = new TPad("pad2", "apd2",0.0,y2,1.,y1);  
    y1=y2;
    c2->Draw();
    c2->cd();
    int i=4;
    TH1 *h = GetHisto(fd,name[i]);
    if (h) {
	h->Draw(); 
	if(h->Integral() > 0) gPad->SetLogy();
    }
  }

  c2->cd(5);
  gPad->SetGridx();

  c->cd(0);
  c3 = new TPad("pad3", "apd3",0.0,0.,1.,y1);
  c3->Draw();
  c3->cd();
  int i=6;
  TH1 *h = GetHisto(fd,name[i]);
  if (h) {
    h->Draw();
    gPad->SetGridx();
  }
}

//--------------------------------------
void eeEmuVsSimu(FileType fd, TPad *c ) {
   const Char_t *name[2]={"HighTowerTriggerCorruption","PatchSumTriggerCorruption"};
   c->Divide(1,2);
   for (int i = 0;i < 2;i++) {
     TH1 *h = GetHisto(fd, name[i]);
     // printf("aaa%d %s %p\n",i,name[i],h);
     if (h) {
       c->cd(1 + i);
       gPad->SetLogz(0);
       h->Draw("colz");
       if (h->Integral() > 0) gPad->SetLogz();
     }
   }
}

//--------------------------------------
void eeDaqTwCr(FileType fd, TPad *c, EemcTwMask *m) { 
  // raw tower crates 1-6
  //  if (ee2Style) ee2Style->cd(); 
  c->Divide(3,2);
  for (int i = 0;i < 6;i++) {
    TH1 *h = GetHisto(fd, Form("cr%d",i+1));
    if (h) {
      c->cd(i + 1);
      gPad->SetLogz(0);
      h->Draw("colz"); 
      h->SetAxisRange(0, 500);
      if (h->Integral() > 0) gPad->SetLogz();
      if (m) {
         TGraphErrors &gr = m->crG2[i];
         if(gr.GetN() > 0) gr.Draw("P");
      }
    }
  }
}

//--------------------------------------
void eeFreq(FileType fd, TPad *c, EemcTwMask *m) {
  const int nh=4;
  const Char_t *name[nh]={"TowHits","Pre1Hits","Pre2Hits","PostHits"};
  c->Divide(1,4);
  for (int i = 0;i < nh;i++) {
    TH1 *h = GetHisto(fd, name[i]);
    if (!h) continue;
    c->cd(1 + i);
    gPad->SetLogz(0);
    h->Draw("colz");
    gPad->SetGrid();
    if (h->Integral() > 0 ) gPad->SetLogz();
    if((i == 0) && m && (m->phiG.GetN() > 0)){ // show hot towers
      m->phiG.Draw("P");
    }
    if (i == 0) addJPphiLimits(h);
  }
}

//--------------------------------------
void eeDaqTwHit(FileType fd, TPad *c) {
  const int nh=4;
  const Char_t *name[nh]={"HTow","HPre1","HPre2","HPost"};
  c->Divide(2,2);
  for (int i = 0;i < nh;i++) {
    TH1 *h = GetHisto(fd, name[i]);
    if (h) {
	c->cd(1 + i);
	gPad->SetLogy(0);
	h->Draw();
	if (h->Integral() > 0) gPad->SetLogy();
    }
  }
}

//--------------------------------------
void eeMany1D(FileType fd, TPad *c, const Char_t *core, int nh, int nx, int ny) {
  int linLog=1; 
  c->Divide(nx,ny);
  for (int i = 0;i < nh;i++) {
    TH1 *h = GetHisto(fd, Form("%s%d",core,i+1));
    if (h) {
        c->cd(i + 1);
	h->Draw();
        gPad->SetLogy(0);    
	if ((h->Integral() > 0) && (linLog == 1)) gPad->SetLogy();
    }
  }
}

//--------------------------------------
void eeDaqTwHot(FileType fd, TPad *c, EemcTwMask *m) { 
  const int ncr=6; // raw tower crates 1-6
  TH1 *hh[ncr] = {0};
  float ymax = 2.0;
  c->Divide(1, ncr);
  for (int i = 0;i < ncr;i++) {
    TH1 *h = GetHisto(fd, Form("cr%dHot",i+1));
    if (h) {
	hh[i] = h;
	c->cd(i + 1);
	gPad->SetLogy(0);
	gPad->SetGridx();
	h->Draw("b");
	if (h->Integral() <= 10) continue;
	if (h->GetRMS() <= 2.0) continue;
	if (h->Integral() > 1) gPad->SetLogy();
	h->Fit("pol0");
	TF1 *ff = h->GetFunction("pol0");
	float yM = ff->GetParameter(0);
	if (ymax < yM) ymax = yM;
	if (m) {
	    TGraph &gr = m->crG[i];
	    if (gr.GetN() > 0) gr.Draw("P");
	}
    }
 }
 for (int i = 0;i < ncr;i++) if (hh[i]) hh[i]->SetMaximum(ymax * 20.);
}

//--------------------------------------
void eeDaqMapmtCr(FileType fd, TPad *c, int cr1) {
  //raw  mapmt crates 84-91 or 92-99 
  c->Divide(4,2);
  for (int cr = cr1;cr <= cr1 + 7;cr++) {
    TH1 *h = GetHisto(fd, Form("cr%d", cr));
    if (h) {
	c->cd(cr - cr1 + 1);
	gPad->SetLogz(0);
	h->Draw("colz"); 
	h->SetAxisRange(0, 1000);
	if(h->Integral() > 0) gPad->SetLogz();
    }
  }
}

//--------------------------------------
void eeDaqSmdA(FileType fd, TPad *c, const Char_t *core, Char_t uv){
  if(strstr(core, "SmdA")) {
    c->Divide(2, 6);
  } else {
    c->Divide(3, 4);
  }
  for (int sec = 1;sec <= 12;sec++) {
    TH1 *h = GetHisto(fd, Form("%s%d%c",core,sec,uv));
    if (h) {
	c->cd(sec);
	h->Draw();
        gPad->SetLogy(0);
	if (h->Integral() > 0) gPad->SetLogy();
    }
  }
}

//--------------------------------------
void eeDaqMapmtStat(FileType fd, TPad *c) {
  c->Divide(1,2);
  TH1 *h = GetHisto(fd, "MAPMHits");
  TH1 *hcopy = GetHisto(fd, "MAPMHits");
  if (h) {
    c->cd(1);
    h->SetAxisRange(63, 87);
    h->SetXTitle("Crate ID     12S1=64,  1S1=68,  2S1=72,  3S1=76,  4S1=80,  5S1=84"); 
    h->Draw("colz");
    gPad->SetGrid();
    gPad->SetLogz(0);    
    if (h->Integral()>0) gPad->SetLogz();
  }
  if (hcopy) {
    c->cd(2);
    hcopy->SetAxisRange(88, 120);
    hcopy->SetXTitle("Crate ID     6S1=88,  7S1=92,  8S1=96,  9S1=100, 10S1=104,  11S1=108");
    hcopy->Draw("colz");
    gPad->SetGrid();
    gPad->SetLogz(0);    
    if (hcopy->Integral()>0) gPad->SetLogz();
  }
}

//--------------------------------------
void eeTrigHanks(FileType fd, TPad *c ) {
  const Char_t *name[2]={"dsm0inJPall_HT","dsm0inJPall_TP"};
  c->Divide(1,2);
  for (int i = 0;i < 2;i++) {
    TH1 *h = GetHisto(fd, name[i]);
    if (h) {
	c->cd(1 + i);
	gPad->SetLogz(0);
	h->Draw("colz");
	if (h->Integral() > 0) gPad->SetLogz();
    }
  }
}

//--------------------------------------
void eeTrigDsm0(FileType fd, TPad *c, const Char_t *mode ) {
  c->Divide(2,3);
  const int ncr = 6;
  TH1 *hh[ncr] = {0};
  float ymax = 0;
  for (int j = 0;j < ncr;j++) {
    c->cd(j + 1);
    TH1 *h = GetHisto(fd, Form("dsm0inJP%d_%s", j+1, mode));
    if (h) {
	hh[j] = h;
	gPad->SetLogz(0);
	h->Draw("colz");
	if (ymax < h->GetMaximum()) ymax = h->GetMaximum();
	if (h->Integral() > 0) gPad->SetLogz();
    }
  }
  for (int j = 0;j < ncr;j++) if (hh[j]) hh[j]->SetMaximum(ymax);
}

//--------------------------------------
void eeTrigDsm1(FileType fd, TPad *c, const Char_t *mode ) {
  const Char_t *core="dsm1HJP";
  if (mode[0] == 'H') {
    c->Divide(2, 6);
  } else {
    c->Divide(4, 3);
  }
  const int n = 12;
  TH1 *hh[n] = {0};
  float ymax = 0;
  for (int j = 0;j < n;j++) { 
    TH1 *h = GetHisto(fd, Form("%s%d_%s",core,j+1,mode));
    if (h) {
	hh[j] = h;
	c->cd(j + 1);
	h->Draw("colz");
	gPad->SetLogz(0);
	if (ymax < h->GetMaximum()) ymax = h->GetMaximum();
	if (h->Integral() > 0) gPad->SetLogz();
	if(mode[0] == 'H') gPad->SetGridx();
    }
 }
 for (int j = 0;j < n;j++) if (hh[j]) hh[j]->SetMaximum(ymax);
}

//--------------------------------------
void eeTrigDsm2HT(FileType fd, TPad *c ) {
  const Char_t *name[3]={"dsm2Half1_HTTP","dsm2Half2_HTTP","dsm3_HTTP"};
  c->Divide(2, 2);
  for (int i = 0;i < 3;i++) {
    TH1 *h = GetHisto(fd,name[i]);
    if (h) {
	c->cd(1 + i);
	gPad->SetLogz(0);
	h->Draw("colz");
	if (h->Integral() > 0) gPad->SetLogz();
    }
  }
}
  
//--------------------------------------
void eeTrigJPsum(FileType fd, TPad *c, const Char_t *mode) {
  c->Divide(2, 3);
  const Char_t *core="JP";
  for (int j = 0;j < 6;j++) { 
    TH1 *h = GetHisto(fd, Form("%s%d%s", core, j + 1, mode));
    if (h) {
	c->cd(j + 1);
	int maxbin = h->GetMaximumBin() - 1;
	const Char_t *title = h->GetTitle();
	h->SetTitle(Form("%s    ped= %d\n", title, maxbin));
        gPad->SetLogy(0);
	h->GetXaxis()->SetRange(1, 200); //for p-p commnet out and use full range for Au-Au
	h->Draw();
	if (h->Integral() > 0) gPad->SetLogy();
    }
  }
}

//--------------------------------------
void eeTrigJPfreq(FileType fd, TPad *c) {
  c->Divide(2, 3);
  const Char_t *core="JPsumTh";
  for (int j = 0;j < 4;j++) { 
    TH1 *h = GetHisto(fd, Form("%s%d",core,j)); 
    if (h) {
	c->cd(j + 1);
        h->Draw();
	h->SetMinimum(0);
    }
  }
  for (int j = 0;j < 2;j++) { 
    TH1 *h = GetHisto(fd, Form("dsm2Half%d_Etot",j+1)); 
    if (h) {
	c->cd(j + 5);
	h->Draw();
    }
  }
#if 0 
  // out 2006+
  TH1 *h = GetHisto(fd, "JPadjTh"); 
  c->cd(5);
  if (h) h->Draw();
#endif
}

//--------------------------------------
void eeTrigAdjJPsum(FileType fd, TPad *c, const Char_t *mode) {
  c->Divide(2, 3);
  const Char_t *core="JP";
  for (int j = 0;j < 6;j++) {
    TH1 *h = GetHisto(fd, Form("%s%d%d%s",core,j+1,((j+1)%6)+1,mode)); 
    if (h) {
	c->cd(j + 1);
	gPad->SetLogy(0);
	// h->GetXaxis()->SetRange(19,69); //for p-p commnet out and use full range for Au-Au
	h->Draw();
	if (h->Integral() > 0) gPad->SetLogy();
    }
  }
}

//--------------------------------------
void eeTrigEtot(FileType fd, TPad *c) {
  const Char_t *nameA[3]={"dsm2E_etot","dsm2B_etot","dsm2BE_etot"};
  c->Divide(1, 3);
  const int n = 6;
  TH1 *hh[n] = {0};
  float ymax = 0;
  for (int i = 0;i < 3;i++) {
    c->cd(1 + i);
    gPad->SetLogy(0);
    for (int ii = 0;ii <= 1;ii++) {  
	TH1 *h = GetHisto(fd, Form("%s%d",nameA[i],ii));
	if (h) {
	    hh[i + (ii * 3)] = h;
	    if (ii == 0) {
		h->Draw();
		TString tt = h->GetTitle();
		tt += "  COLORS:   bit=0 BLUE , bit=1 RED";
		h->SetTitle(tt);
    	    } else {
		h->Draw("same");
	    }
    	    if ((ii == 0) && (h->Integral() > 0)) gPad->SetLogy();
	    if (ymax < h->GetMaximum()) ymax = h->GetMaximum();
        }
    }
  }
  for (int j = 0;j < n;j++) if (hh[j]) hh[j]->SetMaximum(ymax * 1.1);
}
  
//--------------------------------------
void eeTrigAdjJPcor(FileType fd, TPad *c, const Char_t *mode) {
  c->Divide(3, 2);
  const Char_t *core="JP";
  for (int j = 0;j < 6;j++) { 
    TH1 *h = GetHisto(fd, Form("%s%d%d%s",core,j+1,((j+1)%6)+1,mode)); 
    if (h) {
	c->cd(j + 1);
	gPad->SetLogz(0);
	h->Draw("colz");
	if (h->Integral() > 0) gPad->SetLogz();
    }
  }
}

//--------------------------------------
void addJPphiLimits(TH1 *h) {
  TList *Lx = h ? h->GetListOfFunctions() : 0;
  if (!Lx || (Lx->GetSize() > 0)) return; // do it just once
  for (int i = 0;i <= 5;i++) { // boundarioes for JP
    float x1 = 2.5 + (i * 10);
    TLine *ln = new TLine(x1, -0.2, x1, 12.5);
    if (ln) {
	// no need to garbage collect these because h->GetListOfFunctions() will clean them by itself
	//if (hCleanUp) hCleanUp->Add(ln);
	Lx->Add(ln);
    }
    int jpid = i + 2;
    if (jpid == 7) jpid = 1;
    TString aa = "Jet Patch "; aa += jpid;
    TText *tt = new TText(x1 + 1, 12.6, aa);
    if (tt) {
	//if (hCleanUp) hCleanUp->Add(tt);
	tt->SetTextSize(0.08);
	Lx->Add(tt);
    }
  }
}

//--------------------------------------
void eeJpQaMinMax(TH1 *hh) {
    if (!hh) return;
    hh->SetAxisRange(0.5, 6.4);
    hh->SetMinimum(0.);
    int ib = hh->GetMaximumBin();
    float yMax = hh->GetBinContent(ib);
    ib = hh->GetMinimumBin();
    float yMin = hh->GetBinContent(ib);
    float r = 0, er = 999;
    if (yMin <= 0) yMin = 1;
    if (yMax <= 0) yMax = 1;    
    if ((yMin > 0) && (yMax > 0)) {
	r = yMin / yMax;
	er = r * TMath::Sqrt((1 / yMax) + (1 / yMin));
	LOG_DEBUG << Form("JP min/max=%.2f +/- %.2f  (min=%.0f max=%.0f) \"%s\"", r, er, yMin, yMax, hh->GetTitle()) << endm;
	LOG_DEBUG << Form("#JP %.2f %.2f %.0f %.0f :%s", r, er, yMin, yMax, hh->GetTitle()) << endm;
    }
}
