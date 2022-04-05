#include "TSystem.h"
#include "TString.h"

void RunPythia(const char *jobid="test",const char *bin)
{
  Int_t nev=99999999;

  TString input("/star/u/russcher/gamma/analysis/data/pythia/");
  input+=TString(bin);
  input+=TString(".root");
  TString outdir("/star/u/russcher/gamma/analysis/output/pythia/");
  outdir=outdir+TString(bin)+"_"+TString(jobid)+"/";
  TString psout("pi0_pythia.ps");
  TString psout2("pi0_pythia2.ps");
  TString rootout("pi0_pythia.root");
  psout.Prepend(bin);
  psout2.Prepend(bin);
  rootout.Prepend(bin);
  psout.Prepend(jobid);
  psout2.Prepend(jobid);
  rootout.Prepend(jobid);
  TString command("mkdir ");
  command.Append(outdir.Data());
  gSystem->Exec(command.Data());
  cout<<endl<<"storing results in: "<<command.Data()<<endl<<endl;
  
  psout.Prepend(outdir.Data());
  psout2.Prepend(outdir.Data());
  rootout.Prepend(outdir.Data());

  gSystem->Load("$HOME/MyEvent/MyEvent.so");
  gSystem->Load("$HOME/gamma/analysis/lib/AnaCuts.so");
  gSystem->Load("$HOME/gamma/analysis/lib/EventMixer.so");
  gSystem->Load("$HOME/gamma/analysis/lib/Pi0Analysis.so");

  Pi0Analysis *pi0=new Pi0Analysis(psout.Data(),psout2.Data(),"pp05");
  pi0->setMC(kTRUE);
  pi0->setPythia(kTRUE);
  pi0->init(rootout.Data());
  pi0->make(nev,input.Data());
  pi0->getYield();
  pi0->finish();
}
