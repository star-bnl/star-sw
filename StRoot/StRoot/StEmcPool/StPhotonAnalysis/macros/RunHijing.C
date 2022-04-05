#include "TSystem.h"
#include "TString.h"

void RunHijing(const char *jobid="test")
{
  Int_t nev=99999999;

  TString input("/star/u/russcher/gamma/analysis/data/hijing/hijing.root");
  
  TString outdir("/star/u/russcher/gamma/analysis/output/dAu/hijing/");
  TString psout("pi0_hijing.ps");
  TString rootout("pi0_hijing.root");
  psout.Prepend(jobid);
  rootout.Prepend(jobid);
  TString command("mkdir ");
  command.Append(outdir.Data());
  command.Append(jobid);
  gSystem->Exec(command.Data());
  cout<<endl<<"storing results in: "<<command.Data()<<endl<<endl;
  outdir.Append(jobid);
  outdir.Append("/");

  psout.Prepend(outdir.Data());
  rootout.Prepend(outdir.Data());

  gSystem->Load("MyEvent/MyEvent.so");
  gSystem->Load("gamma/analysis/lib/AnaCuts.so");
  gSystem->Load("gamma/analysis/lib/Pi0Analysis.so");

  Pi0Analysis *pi0=new Pi0Analysis(psout.Data(),"dAu");
  pi0->setMC(kFALSE);
  pi0->setHijing(kTRUE);
  pi0->init(rootout.Data());
  pi0->make(nev,input.Data());

  TH1F *h_mb=new TH1F(*pi0->getYield(0,"mb"));
  for(int i=1;i<=h_mb->GetNbinsX();i++){
    h_mb->SetBinContent(i,h_mb->GetBinContent(i)*h_mb->GetXaxis()->GetBinWidth(i));
    h_mb->SetBinError(i,h_mb->GetBinError(i)*h_mb->GetXaxis()->GetBinWidth(i));
  }
  TH1F *h_input=new TH1F(*(TH1F*)pi0->h_mcneutronsMB);

  TCanvas *c=new TCanvas("c","c",400,600);
  c->Divide(1,2);
  c->cd(1);
  gPad->SetLogy();
  h_input->Draw();
  h_mb->Draw("same");
  h_mb->SetLineStyle(2);
  c->cd(2);
  TH1F *eff=new TH1F(*h_mb);
  eff->Divide(h_input);
  eff->Draw();
  c->cd(0);
  c->SaveAs("hijing.ps");

  pi0->finish();
}
