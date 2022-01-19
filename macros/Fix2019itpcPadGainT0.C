/* 
   root.exe lDb.C itpcPadGainT0.20190119.000134.root Fix2019itpcPadGainT0.C
*/
void Fix2019itpcPadGainT0(const Char_t *newf = "itpcPadGainT0.20190119.000135.root") {
  struct deadPad_t {
    Int_t sector;
    Int_t row;
    Int_t padMin;
    Int_t padMax;
  };
  deadPad_t pads[] = {
    // 
    { 1, 36, 17, 17},
#if 0
    { 7, 55, 14, 14}, // hot pad
    { 7, 56, 14, 14}, // hot pad
    {12, 26, 41, 41}, // hot pad
    {12, 43, 19, 19}, // hot pad
    {14, 38, 41, 41}, // hot pad
    //{17, 31-35, 20, 40}, // possible time depended problem region
#endif
    {19, 13,  5, 22},
    {19, 14,  2, 22},
    {19, 15,  9, 22},
    {19, 16, 14, 23},
    {20,  7,  2,  3},
    {20,  8,  2,  7},
    {20,  9,  2, 25},
    {20, 10,  2, 25}
  };
  St_itpcPadGainT0 *table = (St_itpcPadGainT0 *) gDirectory->Get("itpcPadGainT0");
  itpcPadGainT0_st *g = table->GetTable();
  Int_t N = sizeof(pads)/sizeof(deadPad_t);
  cout << "Found " << N << " dead pads" << endl;
  for (Int_t i = 0; i < N; i++) {
    Int_t s = pads[i].sector;
    Int_t r = pads[i].row;
    Int_t padMin = pads[i].padMin;
    Int_t padMax = pads[i].padMax;
    cout << "Reset Gains for sector = " << s << " row = " << r << " pads in range [" << padMin << "," << padMax << "]" << endl;
    if (s < 1 || s > 24) continue;
    if (r < 1 || r > 40) continue;
    if (padMin > padMax) continue;
    if (padMin < 1 || padMax > 144) continue;
    for (Int_t pad = padMin; pad <= padMax; pad++) {
      cout << "Reset gain[" << s - 1 << "][" << r - 1 << "][" << pad -1 << "] = " << g->Gain[s-1][r-1][pad-1] << " to 0" << endl;
      g->Gain[s-1][r-1][pad-1] = 0;
    }
  }
  TFile *f = new TFile(newf,"recreate");
  table->Write();
  delete f;
}
