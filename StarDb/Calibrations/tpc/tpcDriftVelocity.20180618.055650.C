TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 169004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54659; // +/- 6.40154e-06 cm/us All: East = -0.443648 +/- 0.00287899
  row.laserDriftVelocityWest	 =   5.54659; // +/- 6.40154e-06 cm/us All: West = 0.300584 +/- 0.0012489
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54659 +/- 6.40154e-06
  return (TDataSet *)tableSet;// West = 5.54595 +/- 6.96834e-06 East = 5.55007 +/- 1.62045e-05
};
