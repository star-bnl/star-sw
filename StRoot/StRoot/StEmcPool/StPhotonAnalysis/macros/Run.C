#include "TSystem.h"
#include "TString.h"

void Run(const char *jobid="test")
{
  Int_t nev=99999999;

  TString input0("/star/u/russcher/gamma/analysis/data/dAu/dAuCombined_0.root");
  TString input1("/star/u/russcher/gamma/analysis/data/dAu/dAuCombined_1.root");
  TString input2("/star/u/russcher/gamma/analysis/data/dAu/UPCCombined.root");  
  TString input3("/star/u/russcher/gamma/analysis/data/dAu/dAuUPCCombined.root");

  TString outdir("/star/u/russcher/gamma/analysis/output/dAu/");
  TString psout("pi0_dAu.ps");
  TString rootout("pi0_dAu.root");
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

  gSystem->Load("$HOME/MyEvent/MyEvent.so");
  gSystem->Load("$HOME/gamma/analysis/lib/AnaCuts.so");
  gSystem->Load("$HOME/gamma/analysis/lib/EventMixer.so");
  gSystem->Load("$HOME/gamma/analysis/lib/Pi0Analysis.so");
  
  Pi0Analysis *pi0=new Pi0Analysis(psout.Data(),"/dev/null","dAu");
  pi0->setMC(kFALSE);
  pi0->init(rootout.Data());
  
  pi0->make(nev,input0.Data());
  pi0->make(nev,input1.Data());
  pi0->printPrescales();
  //pi0->setNoMINBIAS(kTRUE);
  pi0->make(nev,input2.Data());
  pi0->printPrescales();
  pi0->make(nev,input3.Data());
  pi0->printPrescales();
  
  pi0->getYield();
  pi0->finish();
}
