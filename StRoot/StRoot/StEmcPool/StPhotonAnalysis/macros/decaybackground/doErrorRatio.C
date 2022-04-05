void doErrorRatio(char *file,char *fileplus,char *filemin){
  
  TFile *f_nom=new TFile(file,"OPEN");
  TFile *f_plus=new TFile(fileplus,"OPEN");
  TFile *f_min=new TFile(filemin,"OPEN");
  TH1F *h_nom=(TH1F*)f_nom->Get("gamma");
  TH1F *h_plus=(TH1F*)f_plus->Get("gamma");
  TH1F *h_min=(TH1F*)f_min->Get("gamma");

  for(Int_t i=1;i<=h_nom->GetNbinsX();i++){
    Float_t error=fabs(h_plus->GetBinContent(i)-h_min->GetBinContent(i))/2.;
    cout<<i<<" err: "<<error<<endl;
    h_nom->SetBinError(i,error);
  }

  TCanvas *c=new TCanvas("c","c",400,300);
  TH1F *h_divplus=new TH1F(*h_plus);
  h_divplus->Divide(h_nom);
  h_divplus->SetLineColor(4);
  h_divplus->SetLineWidth(2);
  h_divplus->SetMaximum(1.1);
  h_divplus->SetMinimum(.9);
  h_divplus->SetTitle("error on ratio from fit;p_{T}");
  h_divplus->Draw("hist");
  TH1F *h_divmin=new TH1F(*h_min);
  h_divmin->Divide(h_nom);
  h_divmin->SetLineColor(4);
  h_divmin->SetLineWidth(2);
  h_divmin->Draw("histsame");
  c->SaveAs("gammaDecayWithErrors2sigma.root");
  c->SaveAs("gammaDecayWithErrors2sigma.pdf");

}
