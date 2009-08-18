#include "TSystem.h"
#include "TString.h"

void RunPP(const char *jobid="test")
{
  Int_t nev=99999999;

  //TString input0("/star/u/russcher/gamma/analysis/data/pp05/ppProduction.root");
  TString input0("/star/u/russcher/gamma/analysis/data/pp05/ppProduction_rcf_0.root");
  TString input1("/star/u/russcher/gamma/analysis/data/pp05/ppProduction_rcf_1.root");
  TString input2("/star/u/russcher/gamma/analysis/data/pp05/ppProduction_rcf_2.root");
  TString input3("/star/u/russcher/gamma/analysis/data/pp05/ppProduction_rcf_3.root");
  TString input4("/star/u/russcher/gamma/analysis/data/pp05/ppProductionMinBias.root");

  TString outdir("/star/u/russcher/gamma/analysis/output/pp05/");
  TString psout("pi0_pp05.ps");
  TString psout2("eta_pp05.ps");
  TString rootout("pi0_pp05.root");
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
  psout2.Prepend(outdir.Data());
  rootout.Prepend(outdir.Data());

  gSystem->Load("$HOME/MyEvent/MyEvent.so");
  gSystem->Load("$HOME/gamma/analysis/lib/AnaCuts.so");
  gSystem->Load("$HOME/gamma/analysis/lib/EventMixer.so");
  gSystem->Load("$HOME/gamma/analysis/lib/Pi0Analysis.so");
  

  Pi0Analysis *pi0=new Pi0Analysis(psout.Data(),psout2.Data(),"pp05");
  pi0->setMC(kFALSE);
  pi0->init(rootout.Data());
  
  pi0->make(nev,input0.Data());
  pi0->make(nev,input1.Data());
  pi0->make(nev,input2.Data());
  pi0->make(nev,input3.Data());
  pi0->printPrescales();
  cout<<"****** starting with minbias *********"<<endl;
  pi0->make(nev,input4.Data());
  pi0->printPrescales();

  pi0->getYield();
  pi0->finish();
}
