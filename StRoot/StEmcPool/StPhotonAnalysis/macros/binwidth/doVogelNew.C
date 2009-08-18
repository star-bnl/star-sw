//-----------------------------------------------------
//* Fit pQCD and calculate corrections finite bin width
//-----------------------------------------------------
void doVogelNew(int trig,const char *flag)
{
  gSystem->Load("$HOME/gamma/analysis/lib/AnaCuts");

  AnaCuts *cuts=new AnaCuts(flag);

  char *gifout;
  Int_t nbins;
  TArrayD bins;
  TH1F *h;

  if(trig==1){
    nbins=cuts->nPtBinsMB;
    bins=cuts->ptBinsMB;
    h=new TH1F("h","h",nbins,bins.GetArray());
    if(strcmp(flag,"pp05")==0) gifout="mbbincorrPP.gif";  
    if(strcmp(flag,"dAu")==0) gifout="mbbincorrDAU.gif";
  }
  else if(trig==2){
    nbins=cuts->nPtBinsHT1;
    bins=cuts->ptBinsHT1;
    h=new TH1F("h","h",nbins,bins.GetArray());
    if(strcmp(flag,"pp05")==0) gifout="ht1bincorrPP.gif";
    if(strcmp(flag,"dAu")==0) gifout="ht1bincorrDAU.gif";
  }
  else if(trig==3){
    nbins=cuts->nPtBinsHT2;
    bins=cuts->ptBinsHT2;
    h=new TH1F("h","h",nbins,bins.GetArray());
    if(strcmp(flag,"pp05")==0) gifout="ht2bincorrPP.gif";
    if(strcmp(flag,"dAu")==0) gifout="ht2bincorrDAU.gif";
  }
  else
    cout<<"error"<<endl;
  

  for(Int_t b=1;b<=nbins;b++){
    Float_t xmin=bins[b-1];
    Float_t xmax=bins[b];
    Float_t dpT=xmax-xmin;
    Float_t pT=xmin + 0.5*dpT;
    
    
    TF1 *fit=new TF1("fit","[0]*pow(1.+x,[1])*pow(x,[2])",1.,15.);
    if(strcmp(flag,"pp05")==0) fit->SetParameters(1.,-9.3,0.);
    if(strcmp(flag,"dAu")==0) fit->SetParameters(1.,-9.5,0.);

    cout<<"using exponent: "<<fit->GetParameter(1)<<endl;
    //now this is N(p_{T})
    TF1 *N=new TF1(*fit);
    N->SetParameter(2,1.);
    //
    cout<<xmin<<" "<<xmax<<" at "<<pT<<" and "<<dpT<<endl;
    Float_t ratio=fit->Eval(pT)/(N->Integral(xmin,xmax)/(pT*dpT));
    h->Fill(pT,ratio);
  }

  h->SetBinContent(1,0);
  h->SetMaximum(1.2);
  h->SetMinimum(.6);
  
  TCanvas *cc=new TCanvas("cc","cc",300,300);
  h->Draw();
  cc->SaveAs(gifout);

  char *hname;
  if(trig==1) hname="h4mb";
  else if(trig==2) hname="h4ht1";
  else if(trig==3) hname="h4ht2";
  else return;
  TFile *outf;
  if(strcmp(flag,"pp05")==0) outf=new TFile("bincorrectionsPP.root","UPDATE");
  if(strcmp(flag,"dAu")==0) outf=new TFile("bincorrectionsDAU.root","UPDATE");
  h->Write(hname,2,0);
  outf->Close();
}


