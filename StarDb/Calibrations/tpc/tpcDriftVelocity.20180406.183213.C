TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54512; // +/- 2.05007e-05 cm/us All: East = -4.31008 +/- 0.00433831
  row.laserDriftVelocityWest	 =   5.54512; // +/- 2.05007e-05 cm/us All: West = -3.44616 +/- 0.00775114
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54512 +/- 2.05007e-05
  return (TDataSet *)tableSet;// West = 5.5417 +/- 3.94479e-05 East = 5.54638 +/- 2.39955e-05
};
