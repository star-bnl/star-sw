//#define SCALE
TFile *f = 0;

void Gain() {//St_db_Maker *dbMk=0){
  //  if (! dbMk) return;
  struct Gain_t {
    Float_t sector;
    Float_t row;
    Float_t pad;
    Float_t Gain;
  };
  Gain_t G;
  gROOT->LoadMacro("Load.C");
  Load();
  //  f = new TFile("StarDb/Calibrations/tpc/tpcGain.20000614.175430.root");
  //  f = new TFile("./tpcGain.20000614.175430.root");
  //  f = new TFile("./tpcGain.root");
  //  St_tpcGain *gain = (St_tpcGain *) f->Get("tpcGain");
  //  delete f;
//   Char_t *tabNam  = "Calibrations/tpc/tpcGain";
//   TDataSet *tpc = dbMk->GetDataBase(gSystem->DirName(tabNam));
//   if (! tpc) return;
  //  St_tpcGain *tpcGain = (St_tpcGain *) tpc->Find(gSystem->BaseName(tabNam));
  St_tpcGain *tpcGain = (St_tpcGain *) gDirectory->Get("tpcGain");
  if (! tpcGain) return;
#ifndef SCALE
  f = new TFile("GainPulser.root","RECREATE");
  TH1D *gains = new TH1D("gains","Dirstribution of TPC gains",1000,0.,50.);
  ntuple = new TNtuple("GainNt","tpcGain content",
		       "sector:row:pad:Gain");
#else
  f = new TFile("./tpcGain.20000614.175430.root","RECREATE");
#endif
  tpcGain_st *g = tpcGain->GetTable();
  Int_t N=0, NBad = 0;
  for (int sector=0;sector<tpcGain->GetNRows(); sector++,g++) {
    for (int row = 0; row<45; row++) {
      for (int pad = 0; pad < 182; pad++) {
	if (g->Gain[row][pad] == 0) continue;
	if (g->Gain[row][pad] < 0) continue;
#ifdef SCALE
	g->Gain[row][pad] *= 1.135;
#else
	gains->Fill(g->Gain[row][pad]);
	G.sector = sector+1;
	G.row    = row+1;
	G.pad    = pad+1;
        G.Gain   = g->Gain[row][pad];
	ntuple->Fill(&G.sector);
	N++;
	if (G.Gain < 0) { 
	  NBad++;
	  printf("%10i/%10i sector %10.0f row %10.0f pad %10.0f\n",
		N,NBad,G.sector,G.row,G.pad,G.Gain);
	}
#endif
      }
    }
  }
#ifdef SCALE
  gain->Write();
#endif
  f->Write();
  delete f;
}
