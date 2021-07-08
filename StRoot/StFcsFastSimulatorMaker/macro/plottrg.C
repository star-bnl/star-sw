TCanvas *c1;
static TFile* file;
static int runnum, yearday, trgVersion, alg;

const char* cNS[2]={"N","S"};
const char* cName[4]={"All","SIM","DEP","TCU"};
const char* cJet[6]={"A","B","C","D","E","F"};
const char* cAlg[3]={"Akio6","Carl6","5JP"};

char* TRGNAME[16]={"EHT","HHT",
		   "ETOT","HTOT",
		   "JP2",  "EM2",  "HAD2",  "GAM2",  "ELE2",
		   "DiJP1","DiEM1","DiHAD1","DiGAM1","DiELE1",
		   "POR","1"};

int HTMAX=256;
int TOTMAX=1024;
int JPMAX=1024;
int FbFMAX=256;
int SUMMAX=256;

#define safelog(x) ((x > 0) ? log10(x) : 0)

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

void plottrg(int trgver=202204, int run=1, int plt=4, char* thr="0.0100"){
  runnum=run;
  yearday=run/1000;
  trgVersion=trgver;
  switch(trgVersion){
  case 202204: alg=0; break;
  case 202205: alg=1; break;
  case 202206: alg=2; break;
  }
  printf("JP algo %d %s",trgVersion,cAlg[alg]);

  c1 = new TCanvas("c1","FCTRGDQA",50,0,1500,1200);
  gStyle->SetLabelSize(0.1,"xy");
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);
  
  char fname[50];
  if(run>10000000){
    sprintf(fname,"%d/%d.thr%s.trgqa.root",yearday,run,thr);
  }else{
    sprintf(fname,"pythia_jet_vz0_run%d.tv%d.trgqa.root",run,trgver);    
  }
  cout << "Opening "<<fname<<endl;
  file=new TFile(fname,"old");

  TText *t;
  char c[50];
  TH1F *h;
  TH2F *h2;
  char hname[50];
  char name[50];

  gStyle->SetTitleH(0.06);
  gStyle->SetOptTitle(1);
  
  gStyle->SetOptStat(0);
  if(plt==0 || plt==1) {
    c1->Clear();
    c1->Divide(1,2);
    c1->cd(1)->SetLogy();
    for(int i=0; i<16; i++){DsmOut->GetXaxis()->SetBinLabel(i+1,TRGNAME[i]);}    
    DsmOut->SetLineColor(6); DsmOut->SetLineWidth(3); DsmOut->Draw();
    DepOut->SetLineColor(4); DepOut->SetLineWidth(2); DepOut->Draw("same");    
    TcuBit->SetLineColor(2); TcuBit->SetLineWidth(2); TcuBit->Draw("same");    
    t=new TText(0.2,0.95,"STG3Out:"); t->SetNDC(); t->SetTextColor(1); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.5,0.95,"SIM");      t->SetNDC(); t->SetTextColor(6); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.6,0.95,"DEP");      t->SetNDC(); t->SetTextColor(4); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.7,0.95,"TCU");      t->SetNDC(); t->SetTextColor(2); t->SetTextSize(0.06); t->Draw();
    float n = DsmOut->GetBinContent(16);
    float nTcuDep = TcuDep->GetEntries();
    float nSimDep = SimDep->GetEntries();
    if(n>0){
      TcuDep->Scale(1.0/n);
      SimDep->Scale(1.0/n);
    }

    c1->cd(2)->SetLogy();
    float max1=TcuDep->GetMaximum();
    float max2=SimDep->GetMaximum();
    TcuDep->SetLineColor(2); TcuDep->SetLineWidth(2); 
    TcuDep->Draw();
    SimDep->SetLineColor(4); SimDep->SetLineWidth(2); 
    SimDep->Draw("same");
    t=new TText(0.30,0.95,Form("Mismatch       N=%d",(int)n)); t->SetNDC(); t->SetTextColor(1); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.50,0.85,Form("DEP-TCU = %d",(int)nTcuDep)); t->SetNDC(); t->SetTextColor(2); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.50,0.75,Form("SIM-DEP = %d",(int)nSimDep)); t->SetNDC(); t->SetTextColor(4); t->SetTextSize(0.06); t->Draw();    
    png("tcu");
  }

  if(plt==0 || plt==2) {
    c1->Clear();
    c1->Divide(1,2);
    c1->cd(1)->SetLogy(); 
    ETotAll->SetAxisRange(0.0,TOTMAX); 
    ETotAll->Draw();
    ETotSIM->SetLineWidth(4); 
    ETotSIM->SetLineColor(6); ETotSIM->Draw("same");    
    ETotDEP->SetLineColor(4); ETotDEP->Draw("same");    
    ETotTCU->SetLineColor(2); ETotTCU->Draw("same");    
    t=new TText(0.2,0.95,"All event");    t->SetNDC(); t->SetTextColor(1); t->Draw();
    t=new TText(0.3,0.95,"Bit2(ETOT) ON : "); t->SetNDC(); t->SetTextColor(1); t->Draw();
    t=new TText(0.5,0.95,"SIM"); t->SetNDC(); t->SetTextColor(6); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.6,0.95,"DEP"); t->SetNDC(); t->SetTextColor(4); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.7,0.95,"TCU"); t->SetNDC(); t->SetTextColor(2); t->SetTextSize(0.06); t->Draw();
    c1->cd(2)->SetLogy(); 
    HTotAll->SetAxisRange(0.0,TOTMAX); 
    HTotAll->Draw();
    HTotSIM->SetLineWidth(3); 
    HTotSIM->SetLineColor(6); HTotSIM->Draw("same");    
    HTotDEP->SetLineColor(4); HTotDEP->Draw("same");    
    HTotTCU->SetLineColor(2); HTotTCU->Draw("same");    
    t=new TText(0.2,0.95,"All event");    t->SetNDC(); t->SetTextColor(1); t->Draw();
    t=new TText(0.3,0.95,"Bit3(HTOT) ON"); t->SetNDC(); t->SetTextColor(1); t->Draw();
    t=new TText(0.5,0.95,"SIM"); t->SetNDC(); t->SetTextColor(6); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.6,0.95,"DEP"); t->SetNDC(); t->SetTextColor(4); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.7,0.95,"TCU"); t->SetNDC(); t->SetTextColor(2); t->SetTextSize(0.06); t->Draw();
    png("tot");
  }

  if(plt==0 || plt==3) {
    c1->Clear();
    c1->Divide(1,2);
    c1->cd(1)->SetLogy();
    ETotAll->SetAxisRange(0.0,TOTMAX); 
    EHTAll->SetAxisRange(0.0,HTMAX); 
    EHTAll->Draw();
    EHTSIM->SetLineWidth(4); 
    EHTSIM->SetLineColor(6); EHTSIM->Draw("same");    
    EHTDEP->SetLineColor(4); EHTDEP->Draw("same");    
    EHTTCU->SetLineColor(2); EHTTCU->Draw("same");    
    t=new TText(0.2,0.95,"All event");    t->SetNDC(); t->SetTextColor(1); t->Draw();
    t=new TText(0.3,0.95,"Bit0(EHT) ON"); t->SetNDC(); t->SetTextColor(1); t->Draw();
    t=new TText(0.5,0.95,"SIM"); t->SetNDC(); t->SetTextColor(6); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.6,0.95,"DEP"); t->SetNDC(); t->SetTextColor(4); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.7,0.95,"TCU"); t->SetNDC(); t->SetTextColor(2); t->SetTextSize(0.06); t->Draw();
    c1->cd(2)->SetLogy(); 
    HHTAll->SetAxisRange(0.0,HTMAX); 
    HHTAll->Draw();
    HHTSIM->SetLineWidth(4); 
    HHTSIM->SetLineColor(6); HHTSIM->Draw("same");
    HHTDEP->SetLineColor(4); HHTDEP->Draw("same");
    HHTTCU->SetLineColor(2); HHTTCU->Draw("same");
    t=new TText(0.2,0.95,"All event");    t->SetNDC(); t->SetTextColor(1); t->Draw();
    t=new TText(0.3,0.95,"Bit1(HHT) ON"); t->SetNDC(); t->SetTextColor(1); t->Draw();
    t=new TText(0.5,0.95,"SIM"); t->SetNDC(); t->SetTextColor(6); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.6,0.95,"DEP"); t->SetNDC(); t->SetTextColor(4); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.7,0.95,"TCU"); t->SetNDC(); t->SetTextColor(2); t->SetTextSize(0.06); t->Draw();
    png("ht");
  }

  if(plt==0 || plt==4) {
    char hname[100];
    c1->Clear();
    c1->Divide(2,6);
    for(int ns=0;ns<2;ns++){
      for(int j=0; j<6; j++){	
	for(int i=0; i<1; i++){
	  int k=j*2+ns+1;
	  c1->cd(k)->SetLogy();
	  sprintf(hname,"JP%s%s%s",cJet[j],cNS[ns],cName[0]);
	  printf("Getting %s\n",hname);
	  TH1F* h = (TH1F*)file->Get(hname);
	  h->SetAxisRange(0.0,JPMAX); 
	  h->SetLineWidth(4);
	  h->Draw();
	  if(k==1){
	    t=new TText(0.2,0.95,Form("%d %s",trgVersion,cAlg[alg])); t->SetNDC(); t->SetTextSize(0.1); t->Draw();
	  }
	  //t=new TText(0.2,0.95,"All event");  t->SetNDC(); t->SetTextColor(1); t->Draw();
	  //t=new TText(0.3,0.95,"Bit4(JP2) ON"); t->SetNDC(); t->SetTextColor(1); t->Draw();
	  //t=new TText(0.5,0.95,"SIM"); t->SetNDC(); t->SetTextColor(6); t->SetTextSize(0.06); t->Draw();
	  //t=new TText(0.6,0.95,"DEP"); t->SetNDC(); t->SetTextColor(4); t->SetTextSize(0.06); t->Draw();
	  //t=new TText(0.7,0.95,"TCU"); t->SetNDC(); t->SetTextColor(2); t->SetTextSize(0.06); t->Draw();
	}
      }
    }
    png("jp");
  }

  if(plt==0 || plt==5) {
    c1->Clear();
    c1->Divide(1,3);
    c1->cd(1)->SetLogy(); 
    E4b4All->SetAxisRange(0.0,FbFMAX); 
    E4b4All->Draw();
    c1->cd(2)->SetLogy(); 
    H4b4All->SetAxisRange(0.0,FbFMAX); 
    H4b4All->Draw();
    c1->cd(3)->SetLogy(); 
    PORAll->Draw();
    png("4x4");
  }

  if(plt==0 || plt==5) {
    c1->Clear();
    c1->Divide(1,2);
    c1->cd(1)->SetLogy(); 
    SumAll->SetAxisRange(0.0,SUMMAX); 
    SumAll->Draw();
    HadAll->SetLineColor(4); HadAll->Draw("same");
    EMAll ->SetLineColor(3); EMAll ->Draw("same");
    GamAll->SetLineColor(2); GamAll->Draw("same");
    EleAll->SetLineColor(6); EleAll->Draw("same");
    t=new TText(0.2,0.95,"E+H"); t->SetNDC(); t->SetTextColor(1); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.3,0.95,"Had"); t->SetNDC(); t->SetTextColor(4); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.4,0.95,"EM "); t->SetNDC(); t->SetTextColor(3); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.5,0.95,"Gam"); t->SetNDC(); t->SetTextColor(2); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.6,0.95,"Ele"); t->SetNDC(); t->SetTextColor(6); t->SetTextSize(0.06); t->Draw();
    c1->cd(2)->SetLogy();   
    EHRAll->Draw();
    RHadAll->SetLineColor(4); RHadAll->Draw("same");
    REMAll ->SetLineColor(3); REMAll ->Draw("same");
    RGamAll->SetLineColor(2); RGamAll->Draw("same");
    REleAll->SetLineColor(6); REleAll->Draw("same");
    t=new TText(0.2,0.95,"Ratio=E4x4/(E4x4+H4x4)");     t->SetNDC(); t->SetTextColor(1); t->SetTextSize(0.06); t->Draw();
    t=new TText(0.2,0.85,"EM : H4x4*128 >= E4x4*THR");  t->SetNDC(); t->SetTextColor(2); t->SetTextSize(0.05); t->Draw();
    t=new TText(0.2,0.80,"HAD: H4x4*128 <  E4x4*THR");  t->SetNDC(); t->SetTextColor(4); t->SetTextSize(0.05); t->Draw();
    t=new TText(0.2,0.75,"THR=32 R=1/(1+THR/128)=0.8"); t->SetNDC(); t->SetTextColor(1); t->SetTextSize(0.05); t->Draw();
    png("HadGamEle");
  }

  if(plt==0 || plt==5) {
    c1->Clear();
    c1->Divide(1,2);
    c1->cd(1);
    SumEtot->Draw("colz");
    c1->cd(2);
    SumHtot->Draw("colz");
    png("SumTot");
  }
}
