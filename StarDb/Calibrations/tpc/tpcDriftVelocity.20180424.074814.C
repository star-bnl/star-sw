TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 114009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54072; // +/- 1.03845e-05 cm/us All: East = -0.80231 +/- 0.00840647
  row.laserDriftVelocityWest	 =   5.54072; // +/- 1.03845e-05 cm/us All: West = 0.162981 +/- 0.00188676
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54072 +/- 1.03845e-05
  return (TDataSet *)tableSet;// West = 5.5405 +/- 1.06036e-05 East = 5.54597 +/- 5.13493e-05
};
