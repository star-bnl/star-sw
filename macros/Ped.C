//#define SCALE
TFile *f = 0;

void Ped() {//St_db_Maker *dbMk=0){
  //  if (! dbMk) return;
  struct Ped_t {
    Float_t sector;
    Float_t row;
    Float_t pad;
    Float_t Pedestal;
    Float_t Rms;
  };
  Ped_t G;
  St_tpcPedestal *tpcPed = (St_tpcPedestal *) gDirectory->Get("tpcPedestal");
  if (! tpcPed) return;
  f = new TFile("PedPulser.root","RECREATE");
  ntuple = new TNtuple("PedNt","tpcPed content",
		       "sector:row:pad:Pedestal:Rms");
  tpcPedestal_st *g = tpcPed->GetTable();
  Int_t N=0, NBad = 0;
  for (int sector=0;sector<tpcPed->GetNRows(); sector++,g++) {
    for (int row = 0; row<45; row++) {
      for (int pad = 0; pad < 182; pad++) {
	if (g->Pedestal[row][pad] <= 0) continue;
	G.sector = sector+1;
	G.row    = row+1;
	G.pad    = pad+1;
        G.Pedestal   = g->Pedestal[row][pad];
        G.Rms   = g->Rms[row][pad];
	ntuple->Fill(&G.sector);
      }
    }
  }
  f->Write();
  delete f;
}
