void makePythiaPlots(file){

  TFile *f=new TFile(file,"OPEN");

  TH1F *h_vert=(TH1F*)f->Get("h_vzMB");
  TH1F *h_partonpt=(TH1F*)f->Get("h_pythiaPartonPt");
  TH1F *h_pionpt=(TH1F*)f->Get("h_pythiaPions");

  TCanvas *c_1=new TCanvas();
  h_vert->Draw();
  c_1->SaveAs("$HOME/gamma/analysis/output/pythia/vertex.ps");
  TCanvas *c_1=new TCanvas("c_1","c_1",600,300);
  c_1->Divide(2,1);
  c_1->cd(1);
  gPad->SetLogy();
  h_partonpt->Draw();
  c_1->cd(2);
  gPad->SetLogy();
  h_pionpt->Draw();
  c_1->SaveAs("$HOME/gamma/analysis/output/pythia/pythiapt.ps");
  
}
