TCanvas *c1;
static TFile* file;
static int runnum, yearday, trgVersion, alg;

void png(char* name){
  char fname[100];
  if(yearday==0){
    sprintf(fname,"%d_%s_tv%d.png",runnum,name,trgVersion);    
  }else{
    sprintf(fname,"%d/%d.%s.png",yearday,runnum,name);    
  }
  //printf("Saving as %s\n",fname);
  c1->SaveAs(fname);
}

void plotcosmic(int plt=0, int run=22179052){
  runnum=run;
  yearday=run/1000;

  c1 = new TCanvas("c1","FCSCOSMIC",50,0,1500,1200);
  gStyle->SetLabelSize(0.1,"xy");
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);
  
  char fname[50];
  sprintf(fname,"%d/%d.cosmic.root",yearday,run);
  cout << "Opening "<<fname<<endl;
  file=new TFile(fname,"old");

  TText *t;
  char c[50];
  TH1F *h;
  TH2F *h2;
  char hname[50];

  gStyle->SetTitleH(0.06);
  gStyle->SetOptTitle(1);
  
  gStyle->SetOptStat(0);

  if(plt==0 || plt==1) {
    c1->Clear();
    c1->Divide(3,2);
    c1->cd(1)->SetLogy(); Ecal_NTower->Draw();
    c1->cd(2)->SetLogy(); Ecal_SigmaMax->Draw();
    c1->cd(3)->SetLogy(); Ecal_SigmaMin->Draw();
    c1->cd(4)->SetLogz(); Ecal_Sigma->Draw("colz");
    c1->cd(5)->SetLogz(); Ecal_SigmaNtow->Draw("colz");
    c1->cd(6); Ecal_ADC->Draw("colz");
    png("EcalCosmic");
  }
  if(plt==0 || plt==2) {
    c1->Clear();
    c1->Divide(3,2);
    c1->cd(1)->SetLogy(); Hcal_NTower->Draw();
    c1->cd(2)->SetLogy(); Hcal_SigmaMax->Draw();
    c1->cd(3)->SetLogy(); Hcal_SigmaMin->Draw();
    c1->cd(4)->SetLogz(); Hcal_Sigma->Draw("colz");
    c1->cd(5)->SetLogz(); Hcal_SigmaNtow->Draw("colz");
    c1->cd(6); Hcal_ADC->Draw("colz");
    png("HcalCosmic");
  }
}
