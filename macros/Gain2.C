class St_tpcGain;
class tpcGain_st;
TFile *f = 0;

void Gain2() {//St_db_Maker *dbMk=0){
  //  if (! dbMk) return;
  struct Gain_t {
    Float_t sector;
    Float_t row;
    Float_t pad;
    Float_t Gain;
  };
  Gain_t G;
  gROOT->LoadMacro("Load.C");
  Load("St_Tables");
  TSeqCollection *files = gROOT->GetListOfFiles();
  Int_t nn = files->GetSize();
  if (! nn) return;
  St_tpcGain *tpcGain[2];
  tpcGain_st *g[2] = {0, 0};
  TFile *f = 0;
  Int_t N = 0;
  TIter next(files);
  while ( (f = (TFile *) next()) && N < 2 ) { 
    TString F(f->GetName());
    if (! F.Contains("tpcGain")) continue;
    tpcGain[N] = (St_tpcGain *) f->Get("tpcGain");
    if (tpcGain[N]) {
      tpcGain[N]->Print(0,2);
      g[N] = tpcGain[N]->GetTable(); N++;
    }
    //    delete f;
  }
  if (N != 2) return;
  f = new TFile("GainDiff.root","RECREATE");
  ntuple = new TNtuple("GainNt","tpcGain content",
		       "sector:row:pad:Gain");
  for (int sector=0;sector<24; sector++,g[0]++, g[1]++) {
    for (int row = 0; row<45; row++) {
      for (int pad = 0; pad < 182; pad++) {
	if (g[0]->Gain[row][pad] <= 0 || g[1]->Gain[row][pad] <= 0) continue;
	G.sector = sector+1;
	G.row    = row+1;
	G.pad    = pad+1;
        G.Gain   = g[0]->Gain[row][pad] - g[1]->Gain[row][pad];
	ntuple->Fill(&G.sector);
      }
    }
  }
  f->Write();
  delete f;
}
