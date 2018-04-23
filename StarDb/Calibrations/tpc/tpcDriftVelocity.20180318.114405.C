TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 77018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55572; // +/- 1.33141e-05 cm/us All: East = -0.940861 +/- 26.2625
  row.laserDriftVelocityWest	 =   5.55572; // +/- 1.33141e-05 cm/us All: West = 0.221336 +/- 0.00236624
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55572 +/- 1.33141e-05
  return (TDataSet *)tableSet;// West = 5.55572 +/- 1.33141e-05 East = 5.57104 +/- 0.0427894
};
