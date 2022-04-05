void doEff(const char *in,const char *PID,const char *flag)
{
  gSystem->Load("$HOME/MyEvent/MyEvent");
  gSystem->Load("$HOME/gamma/analysis/lib/AnaCuts");
  gSystem->Load("$HOME/gamma/analysis/lib/EventMixer");
  gSystem->Load("$HOME/gamma/analysis/lib/Pi0Analysis");
  gSystem->Load("$HOME/gamma/analysis/lib/Efficiency");

  TString out("/star/u/russcher/gamma/analysis/output/dAu/");
  out.Append(PID);
  TString command("mkdir ");
  command.Append(out.Data());
  gSystem->Exec(command.Data());
  out.Append("/");
  out.Append(PID);
  out.Append(flag);

  Efficiency *eff=new Efficiency(in,out.Data(),"dAu");
  eff->init();
  eff->setUseWeight(kTRUE);
  eff->make(5000000);
  eff->finish();
}
