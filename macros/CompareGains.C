struct BPoint_t {
  Float_t sec, row, pad, gain1, gain2, t01, t02;
};
BPoint_t BPoint;
void CompareGains(St_tpcPadGainT0 *t1 = 0, St_tpcPadGainT0 *t2 = 0) {
  if (! t1 || ! t2) return;
  tpcPadGainT0_st *T1 = t1->GetTable();
  tpcPadGainT0_st *T2 = t2->GetTable();
  TFile *f = new TFile("CompareGains.root","RECREATE");
  TNtuple *FitP = new TNtuple("FitP","Compare Gains","sec:row:pad:gain1:gain2:t01:t02");
  for (Int_t sec = 1; sec <= 24; sec++) {
    BPoint.sec = sec;
    for (Int_t row = 1; row <= 45; row++) {
      BPoint.row = row;
      for (Int_t pad = 1; pad <= 182; pad++) {
	BPoint.pad = pad;
	BPoint.gain1 = T1->Gain[sec-1][row-1][pad-1];
	BPoint.gain2 = T2->Gain[sec-1][row-1][pad-1];
	BPoint.t01 = T1->T0[sec-1][row-1][pad-1];
	BPoint.t02 = T2->T0[sec-1][row-1][pad-1];
	FitP->Fill(&BPoint.sec);
      }
    }
  }
  f->Write();
  delete f;
}
