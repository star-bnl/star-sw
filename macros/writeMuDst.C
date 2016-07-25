class StBFChain;
StBFChain *chain;

void writeMuDst(Int_t nevents  = 1, 
		//		const Char_t *MainFile="/star/rcf/test/new/daq_sl302/year_2005/CuCu200_HighTower/st_physics_6054016_raw_1020005.event.root") {
		const Char_t *MainFile="./st_zerobias_adc_11065007_raw_1570001.event.root") {
  gROOT->LoadMacro("bfc.C");
  bfc(-1,"in,StEvent,StDbT,CMuDST,analysis,tree,nodefault,ReadAll",MainFile,"mudst.root",0);
  if (nevents >= 0)   {
    chain->Init();
    if (nevents > 0) chain->EventLoop(1,nevents);
  }
}
