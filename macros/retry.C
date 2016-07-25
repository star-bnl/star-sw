void retry() {
  gSystem->LoadMacro("bfc.C");
  TString cmd("bfc.C(0,\"trs,srs,fss,y2004a,Idst,l0,tpcI,fcf,ftpc,Tree,SvtCL,svtDb,ITTF,Sti,genvtx,SvtIT,geant,tags,bbcSim,tofsim,emcY2,EEfs,evout,GeantOut,big,fzin,MiniMcMk,clearmem\",\"/star/simu/simu/gstardata/rcf1209/done/rcf1209_01_80evts.fzd\")");
  gInterpreter->ProcessLine(cmd.Data());
  St_db_Maker *db = (St_db_Maker*) chain->Maker("db");
  db->SetDebug(3);
  geant = (St_geant_Maker *) chain->GetMaker("geant");
  for (Int_t i = 1; i < 3; i++) {
    geant->Do("rndm");
    geant->Do("gfile p /star/simu/simu/gstardata/rcf1209/done/rcf1209_01_80evts.fzd");
    geant->Do("rndm 9876 54321");
    gInterpreter->ProcessLine(Form("chain->MakeEvent(); >> %i_dbg3evnts.log",i)); 
  }
}
