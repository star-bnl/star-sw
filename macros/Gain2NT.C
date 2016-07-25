class St_tpcGain;
class tpcGain_st;
TFile *f = 0;

void Gain2NT(St_tpcGain *tpcGain = 0, St_tpcT0 *tpcT0 = 0) {//St_db_Maker *dbMk=0){
  if (! tpcGain) return;
  struct Gain_t {
    Float_t sector;
    Float_t row;
    Float_t pad;
    Float_t Gain;
    Float_t t0;
  };
  Gain_t point;
  tpcGain_st *G = tpcGain->GetTable();
  tpcT0_st *T = 0;
  if ( tpcT0) T = tpcT0->GetTable(); 
  f = new TFile("GainT0.root","RECREATE");
  ntuple = new TNtuple("GainNt","tpcGain content",
		       "sector:row:pad:Gain:t0");
  for (int sector=0;sector<24; sector++,G++) {
    if (T) T++;
    for (int row = 0; row<45; row++) {
      for (int pad = 0; pad < 182; pad++) {
	if (G->Gain[row][pad] <= 0) continue;
	point.sector = sector+1;
	point.row    = row+1;
	point.pad    = pad+1;
        point.Gain   = G->Gain[row][pad];
	if (T)         point.t0     = T->T0[row][pad];
	else   point.t0 = 0;
	ntuple->Fill(&point.sector);
      }
    }
  }
  f->Write();
  delete f;
}
