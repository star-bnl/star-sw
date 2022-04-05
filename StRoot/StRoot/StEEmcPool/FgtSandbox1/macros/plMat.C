const int mxF=2;
char *legA[mxF]={"R37cm ","R55cm"};
int colA[mxF]={kBlack,kRed};
TFile *fdA[mxF];

plMat( int page=10,int pl=0, ){ //1=gif, 2=ps, 3=both
  char *path="radlen_fgt";

  int i;
  for(i=0;i<mxF;i++) {
    char fName[1000];
    sprintf(fName,"%s/radlen_fgt_set%d.root",path,i);
    TFile *fd=new TFile(fName);
    if(!fd->IsOpen()) continue;
    printf("Open  histo file=%s=\n",fName);
    fdA[i]=fd;
  }
  fdA[0]->ls();
  gStyle->SetPalette(1,0);

  switch (page) {
  case 10:
    { // radLen vs. eta
      can=new TCanvas("aa","aa",500,420);  
      TPad *c=makeTitle(can,"radLen vs. eta",page);
      c->cd();
      gStyle->SetOptStat(0);
      char *name1="h3002";
      int i;
      lg=new TLegend(0.15,0.6,0.50,0.90);
      TString head="phi range [0,2pi]";
      lg->SetHeader(head);

      for(i=0;i<2;i++) {
	h=(TH1*)fdA[i]->Get(name1);  assert(h);
	if(i==0) {
	 h->Draw();
	 h->SetTitle("Radiation Length vs eta; eta");
	 h->SetAxisRange(0.7,2.7);
	}   else {
	  h->Draw("same");
	  h->SetLineColor(colA[i]);
	}
	lg->AddEntry(h,legA[i]);
      }
      lg->Draw();
 
    }//--------------------------------------
    break;
   
   
    
  default:
    printf("page=%d NOT defined\n",page);
    
  }  
  TString tit=Form("fgtMat_page%03d",page);
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



plAll(char *core="minb_d1r") {
  int i;
  //  for(i=0; i<=7; i++) plRate(i,core,2);
  for(i=0; i<=11; i++) plRate(i,core,2);
}

  
     

 /*
 .L plRate.C
 plAll("F4035dij");
 plAll("F4035rnd");

cat  F4035dij*ps |ps2pdf - >F4035dij.pdf
cat  F4035rnd*ps |ps2pdf - >F4035rnd.pdf


*/

