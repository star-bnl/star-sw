TCanvas *cw, *cf; // working & final plot
TH1 *hTr; // track histogram;
TH2 *hCh; // chi2/dof histogram;
TH1 *hChm; // mean chi2/dof 
int nR=0; // current run index

float par_refYiled=2000;

void plTpcChi2(int mxR=2) { // # of runs
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(0);
  char *inpList="pp500-hist.list";
  printf(" read %d runs from '%s'\n",mxR, inpList);
  FILE *fd=fopen(inpList,"r"); assert(fd);
  cw=new TCanvas(); cw->Divide(1,3);
  hTr=new TH1F("hTr"," # of glob tracks w/ pT>1 GeV/c per run; run index",mxR,0,mxR+1); hTr->SetMinimum(10);
  hCh=new TH2F("hCh","chi2/dof  glob tracks w/ pT>1 GeV/c per run; run index;chi2/dof",mxR,0,mxR+1,30,0,3.);
  hChm=new TH1F("hTr"," mean chi2/dof glob tracks w/ pT>1 GeV/c per run; run index",mxR,0,mxR+1); hChm->SetMinimum(0.5);
  cw->cd(1); hTr->Draw(); gPad->SetLogy();
  cw->cd(2); hChm->Draw();
  cw->cd(3); hCh->Draw("colz"); hChm->Draw("same");
  
  char txt[1000];
  char *cRun, *cFill;
  int lastF=0;
  while(nR<mxR) {
    int ret= fscanf(fd,"%s",txt);
    if(ret!=1) break;
    char* i=strstr(txt,"/");
    cRun=i+1;
    cRun[9]=NULL;
    cFill=txt;
    cFill[6]=NULL;
    int fill=atoi(cFill+1);
    if(lastF!=fill) {
      lastF=fill;
      drawNewFill(cFill);
    }
    nR++;
    printf("%d %s, %d %s %s %d\n",nR,txt,i-txt,cRun,cFill,fill);    
    addRun(cFill,cRun);

    //break;
  }
 

  for(int ic=2;ic<=3;ic++) {
    cw->cd(ic);
    ln=new TLine(0,1,nR,1); ln->SetLineColor(35); ln->SetLineStyle(2);
    ln->Draw();
  }
}

//=========================================
void addRun( char *cFill, char *cRun) {
  TString pathIn="data-ofl/";
  TString inFile=pathIn+cRun+".wana.hist.root";
  TFile *fd=new TFile(inFile);
  if(fd->IsOpen()==0) return;
  assert(fd->IsOpen());
  //fd->ls();

  TH1F *h1=fd->Get("muTrch2"); assert(h1);
  //cw->cd(1); h1->Draw();
  h1->Rebin(2);
  float nTr=h1->Integral();
  float chm=h1->GetMean();
  hTr->Fill(nR,nTr);
  hChm->Fill(nR,chm);

  float weight=par_refYiled/nTr;

  TAxis *ax=h1->GetXaxis(); assert(ax);
  for(int k=1; k<ax->GetNbins(); k++) {
    float x=h1->GetBinCenter(k);
    float val=h1->GetBinContent(k);
    hCh->Fill(nR,x,val*weight);
    // printf("%d %f %f\n",k,x,val);
  }

  //h2=fd->Get("muTrch2b"); assert(h2);
  //cw->cd(2); h2->Draw("colz");

  fd->Close();
}

//===========================
void drawNewFill(char *cFill) {
  printf("draw fill=%s\n",cFill);
  int off=3;

  //----- mean chi2/dof# of tracks
  ln=new TLine(nR,0,nR,2*par_refYiled); ln->SetLineColor(kMagenta);
  cw->cd(1); ln->Draw();
  tx=new TText(nR+off,20,cFill); tx->SetTextAngle(90); tx->Draw();
  tx->SetTextSize(0.06);

  //----- mean chi2/dof
  ln=new TLine(nR,0,nR,5); ln->SetLineColor(kMagenta);
  cw->cd(2); ln->Draw();
  tx=new TText(nR+off,0.6,cFill); tx->SetTextAngle(90); tx->Draw();
tx->SetTextSize(0.06);

  //----- chi2/dof
  ln=new TLine(nR,0,nR,3); ln->SetLineColor(kMagenta);
  cw->cd(3); ln->Draw();
  tx=new TText(nR+off,2,cFill); tx->SetTextAngle(90); tx->Draw();
  tx->SetTextSize(0.06); tx->SetTextColor(kMagenta);



}

