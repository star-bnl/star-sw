TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_svtHybridDriftVelocity")) return 0;
  const double drift_dist = 29928.; //in microns
  const double drift_10 = 28578.; // less 10 cathods in microns
  const double drift_20 = 27228.; // less 20 cathods in microns
  Int_t N = (8*4 + 12*6 + 16*7)*2;
  St_svtHybridDriftVelocity *tableSet = new St_svtHybridDriftVelocity("svtHybridDriftVelocity",N);
  Int_t NB = 3;
  Int_t NH = 2;
  svtHybridDriftVelocity_st row = {0, 0, 0, 432, 0, 0, 1,  1, 1, 1,   9.988,  0.0, 119.609,  0.0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  Int_t i = 0;
  for (Int_t barrel = 1; barrel <= NB; barrel++) {
    Int_t NL = 8;
    Int_t NW = 4;
    if (barrel == 2) {NL = 12; NW = 6;}
    if (barrel == 3) {NL = 16; NW = 7;}
    for (Int_t ladder = 1; ladder <= NL; ladder++) {
      Int_t layer = 2*barrel - 1 + ladder%2;
      for (Int_t wafer = 1; wafer <= NW; wafer++) {
	for (Int_t hybrid = 1; hybrid <= NH; hybrid++) {
	  row.idx = i+1;
	  row.nrows = N;
	  row.Id =  10*(1000* layer + 100* wafer +  ladder) + hybrid;
	  row.barrel = barrel;
	  row.ladder = ladder;
	  row.wafer  = wafer;
	  row.hybrid = hybrid;
	  row.tmin   = 10;
	  row.tmax   = 120;
	  row.v0     = 2.9928/(row.tmax - row.tmin)*2.5e2;
	  tableSet->AddAt(&row.type, i);
	  i++;
	}
      }
    }
  }
  return (TDataSet *)tableSet;
}
