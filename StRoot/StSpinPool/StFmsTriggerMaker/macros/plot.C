TCanvas *c1;
static TFile* file;
static int runnum, yearday, png, pdf;

static const int NBS=68, NJP=6;
static const char* BSname[68]={
  "ST_A","ST_B","ST_BC","ST_C","ST_CD","ST_D",
  "SM_DD",
  "SB_D","SB_CD","SB_C","SB_BC","SB_B","SB_A",
  "NB_A","NB_B","NB_BC","NB_C","NB_CD","NB_D",
  "NM_DD",
  "NT_D","NT_CD","NT_C","NT_BC","NT_B","NT_A",
  "ST_E","ST_EF","ST_F","ST_G","ST_GH","ST_H","ST_HI","ST_I","ST_IJ","ST_J",
  "SM_JJ",
  "SB_J","SB_IJ","SB_I","SB_HI","SB_H","SB_GH","SB_G","SB_F","SB_EF","SB_E",
  "NB_E","NB_EF","NB_F","NB_G","NB_GH","NB_H","NB_HI","NB_I","NB_IJ","NB_J",
  "NM_JJ",
  "NT_J","NT_IJ","NT_I","NT_HI","NT_H","NT_GH","NT_G","NT_F","NT_EF","NT_E"};
static const char* JPname[6]={"STop","SMid","SBot","NBot","NMid","NTop"};

void save(char* name){
  char fname[100];
  if(png){
    if(yearday==0){
      sprintf(fname,"%d_%s.png",runnum,name);    
    }else{
      sprintf(fname,"%d/%d.%s.png",yearday,runnum,name);    
    }
    c1->SaveAs(fname);
  }  
}

void draw(TH1F* h[], int i, int offset){  
  int color;
  if(offset<26)  {h[i+offset]->GetXaxis()->SetRangeUser(0.0,1000.0);}
  else           {h[i+offset]->GetXaxis()->SetRangeUser(0.0,500.0);}
  if(i<9 ) {color=i+1;}
  else     {color=i+2;}
  h[i+offset]->SetLineColor(color); 
  h[i+offset]->SetLineWidth(2);
  if(i==0) {h[i+offset]->Draw();}
  else     {h[i+offset]->Draw("same");}
  TText *t=new TText(0.7,0.80-0.05*i,BSname[i+offset]); t->SetNDC(); t->SetTextSize(0.05); t->SetTextColor(color); t->Draw();
}

void plot(int run=16064020, int plt=0, int log=1, int png=0){
  runnum=run;
  yearday=run/1000;

  c1 = new TCanvas("c1","FMSTRG",50,0,700,720);
  //gStyle->SetLabelSize(0.1,"xy");
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);
  gStyle->SetOptStat(10);
  gStyle->SetOptTitle(1);

  char fname[50];
  if(run==0) {sprintf(fname,"bbcqa.root");}
  else {sprintf(fname,"%d/fmstrg.%d.root",yearday,run);}
  cout << "Opening "<<fname<<endl;
  file=new TFile(fname,"old");

  if(plt==0 || plt==1) {
    c1->Clear();  c1->Divide(1,3);     
    c1->cd(1); BS3->Draw();
    c1->cd(2); BS2->Draw();
    c1->cd(3); BS1->Draw();
    if(png) save("bs");
  }

  if(plt==0 || plt==2) {
    c1->Clear();  c1->Divide(1,3);     
    c1->cd(1); JP2->Draw();
    c1->cd(2); JP1->Draw();
    c1->cd(3); JP0->Draw();
    if(png) save("jp");
  }

  TH1F *h[68];
  for(int i=0; i<NBS; i++) h[i]=(TH1F*)file->Get(BSname[i]);

  if(plt==0 || plt==3) {
    gStyle->SetOptTitle(0);
    c1->Clear();  c1->Divide(2,2);     
    c1->cd(1)->SetLogy(log); for(int i=0; i<7; i++) draw(h,i,0);
    c1->cd(3)->SetLogy(log); for(int i=0; i<6; i++) draw(h,i,7);
    c1->cd(2)->SetLogy(log); for(int i=0; i<7; i++) draw(h,i,7+6);
    c1->cd(4)->SetLogy(log); for(int i=0; i<6; i++) draw(h,i,7+6+7);
    if(png) save("smbssum");
    gStyle->SetOptTitle(1);
  }

  if(plt==0 || plt==4) {
    gStyle->SetOptTitle(0);
    c1->Clear();  c1->Divide(2,2);     
    c1->cd(1)->SetLogy(log); for(int i=0; i<11; i++) draw(h,i,26);
    c1->cd(3)->SetLogy(log); for(int i=0; i<10; i++) draw(h,i,26+11);
    c1->cd(2)->SetLogy(log); for(int i=0; i<11; i++) draw(h,i,26+11+10);
    c1->cd(4)->SetLogy(log); for(int i=0; i<10; i++) draw(h,i,26+11+10+11);
    if(png) save("smbssum");
    gStyle->SetOptTitle(1);
  }

  if(plt==0 || plt==5) {
    c1->Clear();  c1->Divide(2,3);     
    c1->cd(1)->SetLogy(log); STop->Draw();
    c1->cd(2)->SetLogy(log); NTop->Draw();
    c1->cd(3)->SetLogy(log); SMid->Draw();
    c1->cd(4)->SetLogy(log); NMid->Draw();
    c1->cd(5)->SetLogy(log); SBot->Draw();
    c1->cd(6)->SetLogy(log); NBot->Draw();
    if(png) save("jpsum");
  }

}
