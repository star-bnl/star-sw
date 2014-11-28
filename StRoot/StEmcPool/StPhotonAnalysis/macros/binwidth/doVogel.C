//-----------------------------------------------------
//* Fit pQCD and calculate corrections finite bin width
//-----------------------------------------------------
void doVogel(int trig)
{
  gSystem->Load("gamma/analysis/lib/AnaCuts");

  AnaCuts *cuts=new AnaCuts("dAu");

  char *gifout;
  Int_t nbins;
  TArrayD bins;
  TH1F *h;

  if(trig==1){
    nbins=cuts->nPtBinsMB;
    bins=cuts->ptBinsMB;
    h=new TH1F("h","h",nbins,bins.GetArray());
    gifout="mbbincorr.gif";  
  }
  else if(trig==2){
    nbins=cuts->nPtBinsHT1;
    bins=cuts->ptBinsHT1;
    h=new TH1F("h","h",nbins,bins.GetArray());
    gifout="ht1bincorr.gif";
  }
  else if(trig==3){
    nbins=cuts->nPtBinsHT2;
    bins=cuts->ptBinsHT2;
    h=new TH1F("h","h",nbins,bins.GetArray());
    gifout="ht2bincorr.gif";
  }
  else
    cout<<"error"<<endl;
  

  for(Int_t b=1;b<=nbins;b++){
    Float_t xmin=bins[b-1];
    Float_t xmax=bins[b];
    Float_t dpT=xmax-xmin;
    Float_t pT=xmin + 0.5*dpT;
    
    cout<<xmin<<" "<<xmax<<" "<<dpT<<" "<<pT<<endl;
    
    TF1 *fit=new TF1("fit","[0]*pow(1.+x,[1])*pow(x,[2])",1.,7.);
    if(trig==2) fit->SetRange(4.,10.);
    if(trig==3) fit->SetRange(8.,15.);
    fit->SetParameters(1.,-10.,0.0);
    fit->FixParameter(2,0.);
    
    ifstream in("./pQCD.dat");
    Float_t x[100];
    Float_t y[100];
    Int_t i=0;
    while(i<28){
      if(!in.good()) break;      
      in >> x[i] >> y[i];
      i++;
    }
    
    TGraph *g=new TGraph(i,x,y);
    g->Fit("fit","QR");
    fit->SetRange(0.,15.);      
    TF1 *N=new TF1(*fit);
    N->FixParameter(2,1.);
    
    //now this is N(p_{T})
    Float_t ratio=fit->Eval(pT)/(N->Integral(xmin,xmax)/(pT*dpT));
    h->Fill(pT,ratio);
  }

  h->SetBinContent(1,0);
  h->SetMaximum(1.2);
  h->SetMinimum(.6);
  TCanvas *c=new TCanvas("c","c",800,400);
  c->Divide(2,1);
  c->cd(1);
  gPad->SetLogy();
  g->SetMaximum(10.e+10);
  g->SetMinimum(1.);
  g->Draw("ap");
  g->SetMarkerStyle(8);
  fit->Draw("same");
  c->cd(2);
  h->Draw();
  h->SetLineWidth(2);
  c->cd(0);
  c->SaveAs("vogelsang.eps");

  TCanvas *cc=new TCanvas("cc","cc",300,300);
  h->Draw();
  cc->SaveAs(gifout);

  char *hname;
  if(trig==1) hname="h4mb";
  else if(trig==2) hname="h4ht1";
  else if(trig==3) hname="h4ht2";
  else return;
  TFile *outf=new TFile("bincorrections.root","UPDATE");
  h->Write(hname,2,0);
  outf->Close();
}


