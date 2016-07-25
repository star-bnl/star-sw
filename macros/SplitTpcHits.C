class StChain;
StChain  *Chain=0;
class StBFChain;
StBFChain *chain1, *chain2;
//_____________________________________________________________________
void SplitTpcHits(Int_t Nevents = 1, 
		  // "/star/data55/reco/cucu200/hijing_382/b0_20/minbias/y2005h/gheisha_on/p08if/rcf9061_998_200evts.geant.root", 
		  //		  const Char_t *fileOut = "rcf9061_998_200evts.tpcOut.root") {
		  const Char_t *fileIn = "/star/simu/fisyak/rcf9108/rcf9108_0_100evts.fzd",
		  const Char_t *fileOut = "rcf9108_0_100evts.tpcOut.root") {
  gROOT->LoadMacro("bfc.C");
  if (gClassTable->GetID("StBFChain") < 0) Load();
  Chain = new StChain("SplitTpcHits");
  TString chain1Opt("in,sim_T,nodefault");
  TString FileIN(fileIn);
  if (! FileIN.EndsWith(".root")) {
    if ( gClassTable->GetID("TGiant3") < 0) {
      cout << "You have to run root4star in order to read fzd file" << endl;
      return;
    }
    chain1Opt += ",fzin";
  }
  bfc(-1,chain1Opt,fileIn);
  chain1 = chain;
  chain1->SetName("One"); 
  Chain->cd();
  bfc(-1,"geantOut,nodefault",0,fileOut);
  chain2 = chain;
  StMaker *geant = new StMaker("geant");
  StMaker *tree  = chain2->Maker("outputStream");
  chain2->AddBefore("outputStream",geant)
  Chain->cd();
  if (Nevents < 0) return;
  if (Chain->Init() >=  kStEOF) {Chain->FatalErr(iInit,"on init"); return;}
  Int_t jevNo = 0;
  for (Int_t iev = 0; iev < Nevents; iev++) {
    chain1->MakeEvent();
    cout << Form("QAInfo: Read Event run %d event %d",chain1->GetRunNumber(), chain1->GetEventNumber()) << endl;
    TDataSet *gev = chain1->GetDataSet("geant");
    if (! gev) continue;
    St_g2t_tpc_hit *TpcHits = (St_g2t_tpc_hit *) gev->Find("g2t_tpc_hit");
    if (! TpcHits) continue;
    g2t_tpc_hit_st *tpchit = TpcHits->GetTable();
    Int_t N = TpcHits->GetNRows();
    //    if (N > 10) N = 1000;
    for (Int_t j = 0; j < N; j++, tpchit++) {
      if (tpchit->volume_id > 10000) continue; // skip pseudo pad rows
      if (tpchit->volume_id%100 <= 13) continue;
      chain2->Clear();
      Chain->GetEvtHddr()->SetEventNumber(++jevNo);
      St_g2t_tpc_hit *tpcHits = new St_g2t_tpc_hit("g2t_tpc_hit",1);
      geant->AddData(tpcHits);
      tpcHits->AddAt(tpchit);
      chain2->Make();
#if 0
      cout << Form("QAInfo: Done with sub Event no. %d/%d", iev,jevNo);
      cout << Form(" run %d event %d",chain2->GetRunNumber(), chain2->GetEventNumber()) << endl;
#endif
    }
    cout << Form("QAInfo: Done with Event no. %d", iev) << endl;
		 
  }
}


