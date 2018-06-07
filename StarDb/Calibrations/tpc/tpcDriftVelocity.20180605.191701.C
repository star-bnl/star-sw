TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 156036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5542; // +/- 1.10528e-05 cm/us All: East = -0.343107 +/- 0.00420917
  row.laserDriftVelocityWest	 =   5.5542; // +/- 1.10528e-05 cm/us All: West = 0.33905 +/- 0.00220943
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5542 +/- 1.10528e-05
  return (TDataSet *)tableSet;// West = 5.55347 +/- 1.23221e-05 East = 5.55724 +/- 2.50038e-05
};
