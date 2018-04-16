TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52331; // +/- 1.09811e-05 cm/us All: East = -0.31183 +/- 0.00276769
  row.laserDriftVelocityWest	 =   5.52331; // +/- 1.09811e-05 cm/us All: West = 0.0679037 +/- 0.00278972
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52331 +/- 1.09811e-05
  return (TDataSet *)tableSet;// West = 5.52224 +/- 1.55306e-05 East = 5.52437 +/- 1.55287e-05
};
