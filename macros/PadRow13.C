TFile *f = 0;

void PadRow13(){
  struct Gain_t {
    Float_t sector;
    Float_t row;
    Float_t pad;
    Float_t Gain;
  };
  Gain_t G;
  St_tpcGain *gain = (St_tpcGain *) gDirectory->Get("tpcGain");
#if 1
  f = new TFile("GainPulser.root","RECREATE");
  TH1D *gains = new TH1D("gains","Dirstribution of TPC gains",1000,0.,50.);
  ntuple = new TNtuple("GainNt","tpcGain content",
		       "sector:row:pad:Gain");
#endif
  tpcGain_st *g = gain->GetTable();
  Int_t N=0, NBad = 0;
  for (int sector=0;sector<gain->GetNRows(); sector++,g++) {
    //    for (int row = 0; row<45; row++) {
    for (int row = 12; row<13; row++) {
      for (int pad = 0; pad < 182; pad++) {
	if (g->Gain[row][pad] == 0) continue;
	if (g->Gain[row][pad] < 0) continue;
	//	gains->Fill(g->Gain[row][pad]);
	G.sector = sector+1;
	G.row    = row+1;
	G.pad    = pad+1;
        G.Gain   = g->Gain[row][pad];
#if 1
	ntuple->Fill(&G.sector);
#endif
	N++;
	//	if (G.Gain < 0) { 
	  NBad++;
	  printf("%10i/%10i sector %10.0f row %10.0f pad %10.0f\n",
		N,NBad,G.sector,G.row,G.pad,G.Gain);
	  //	}
      }
    }
  }
#if 0
  f->Write();
  delete f;
#endif
}
