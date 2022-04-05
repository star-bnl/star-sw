void doEffPP05(const char *in,const char *PID,const char *flag)
{
  gSystem->Load("$HOME/MyEvent/MyEvent");
  gSystem->Load("$HOME/gamma/analysis/lib/AnaCuts");
  gSystem->Load("$HOME/gamma/analysis/lib/EventMixer");
  gSystem->Load("$HOME/gamma/analysis/lib/Pi0Analysis");
  gSystem->Load("$HOME/gamma/analysis/lib/Efficiency");

  TString out("/star/u/russcher/gamma/analysis/output/pp05/");
  out.Append(PID);
  TString command("mkdir ");
  command.Append(out.Data());
  gSystem->Exec(command.Data());
  out.Append("/");
  out.Append(PID);
  out.Append(flag);

  Efficiency *eff=new Efficiency(in,out.Data(),"pp05");
  eff->init();
  eff->setUseWeight(kTRUE);
  //eff->setUsePythiaWeight(kTRUE);
  //eff->setUseBbcSpread(kFALSE); //set to true: default
  eff->make(5000000);
  eff->finish();
}
