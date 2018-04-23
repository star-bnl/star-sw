TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55563; // +/- 1.2855e-05 cm/us All: East = -0.0196858 +/- 0.00338835
  row.laserDriftVelocityWest	 =   5.55563; // +/- 1.2855e-05 cm/us All: West = 0.36932 +/- 0.00329236
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55563 +/- 1.2855e-05
  return (TDataSet *)tableSet;// West = 5.55458 +/- 1.81076e-05 East = 5.55669 +/- 1.82526e-05
};
