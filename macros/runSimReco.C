void runSimReco(Int_t nevents=9999, const Char_t *option="svt+tpc,LOW_EM", 
		const Char_t *inputfile="/star/simu/evgen/cucu62/hijing_382/b0_14/minbias/evgen.2.nt", 
		const Char_t *outputfile=0) {
  gROOT->LoadMacro("bfc.C");
  TString Chain("");
  TString Option(option);
  if (Option.Contains("fast",TString::kIgnoreCase))
    Chain = "ntin,y2005d,tfs,sfs,fss,bbcSim,emcY2,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,EvOut,McEvOut,GeantOut,IdTruth,miniMcMk,clearmem,StarMagField,FieldOn,McAna";
  else {
    Chain = "ntin,y2005d,StarMagField,FieldOn,logger,trs,fss,Idst,l0,tpcI,fcf,IdTruth,ftpc,Tree,SvtCL,svtDb,ITTF,Sti,genvtx,tags,EEfs,evout,big,miniMcMk,y2005d,McAna";
    if (Option.Contains("svt",TString::kIgnoreCase)) Chain += ",SvtSlowSim,SvtIT";
  }
  if (Option.Contains("ssd",TString::kIgnoreCase)) Chain += ",ssd,ssdDb,SsdIT";
  bfc(-1,Chain.Data(),inputfile,outputfile);
  St_geant_Maker *geantMk = (St_geant_Maker *) chain->Maker("geant");
  TString cmd("");
  if (Option.Contains("LOW_EM",TString::kIgnoreCase)) 
    cmd += "detp  geom  y2005d  LOW_EM;";
  else 
    cmd += "detp  geom  y2005d  hadr_on;";
  cmd += "detp  track ALL 1 200 0.00001 0.00001;";
  cmd += "vsig  0.1  10.;";
  cmd += "mode  RICH simu 2;";
  geantMk->LoadGeometry(cmd.Data());
  if (nevents >= 0)   {
    chain->Init();
    geantMk->Do("RNDM 1 1");
    chain->EventLoop(1,nevents);
  }
}
