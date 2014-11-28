const int mxF=5;
char *legA[mxF]={"0-0.5 ","0.5-1","1-1.5", "1.5-2","2-2.5"};
int colA[mxF]={kBlack,kRed,kGreen,kBlue,kMagenta};
TFile *fdA[mxF];
;

plMatPhi( int rMax=55,int pl=0, ){ //1=gif, 2=ps, 3=both
  char *path="radlen_fgt";

  int i;
  for(i=0;i<mxF;i++) {
    char fName[1000];
    sprintf(fName,"%s/radlen_fgt_R%dset%d.root",path,rMax,i);
    TFile *fd=new TFile(fName);
    if(!fd->IsOpen()) continue;
    printf("Open  histo file=%s=\n",fName);
    fdA[i]=fd;
  }
  fdA[0]->ls();
  gStyle->SetPalette(1,0);

  // radLen vs. phi
      can=new TCanvas("aa","aa",450,380);  
      TPad *c=makeTitle(can,"radLen vs. eta",0);
      c->cd();
      gStyle->SetOptStat(0);
      char *name1="h4002";
      int i;
      lg=new TLegend(0.15,0.6,0.50,0.90);
      TString head=Form("R max=%d cm",rMax);
      lg->SetHeader(head);

      for(i=0;i<mxF;i++) {
	h=(TH1*)fdA[i]->Get(name1);  assert(h);
	if(i==0) {
	 h->Draw();
	 h->SetTitle("Radiation Length vs phi; phi(rad)");
	 h->SetAxisRange(0.0,3.);
	 if(rMax==12) { h->SetMinimum(2e-3);	 h->SetMaximum(4e-2);}
	 if(rMax==55) { h->SetMinimum(1e-2);	 h->SetMaximum(3.); }
	 gPad->SetLogy();
	}   else {
	  h->Draw("same");
	  h->SetLineColor(colA[i]);
	}
	lg->AddEntry(h,legA[i]);
      }
      lg->Draw();
      TString tit=Form("fgtMat_phi_R%d",rMax);
      
  can->SetTitle(tit);
  can->SetName(tit);
  //  c->Clear();
   
   if(pl&1) can->Print(tit+".gif");
   if(pl&2) can->Print(tit+".ps");
   
}


//------------------------
TPad *makeTitle(TCanvas *c,char *core, int page) {

  c->Range(0,0,1,1);
  TPad *pad0 = new TPad("pad0", "apd0",0.0,0.95,1.,1.);
  pad0->Draw();
  pad0->cd();

  TPaveText *pt = new TPaveText(0,0.,1,1,"br");
  pt->Draw();
  TDatime dt;
  TString txt2=core;
  txt2+=", page=";
  txt2+=page;
  txt2+=",  ";
  txt2+=dt.AsString();
  pt->AddText(txt2);
  txt2="--";
  pt->AddText(txt2);

  c->cd();
  pad = new TPad("pad1", "apd1",0.0,0.0,1,.95);
  pad->Draw();
  return pad;
}

