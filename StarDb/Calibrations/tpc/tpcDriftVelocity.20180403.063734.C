TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51984; // +/- 8.91544e-06 cm/us All: East = -0.50923 +/- 0.00390832
  row.laserDriftVelocityWest	 =   5.51984; // +/- 8.91544e-06 cm/us All: West = 0.284888 +/- 0.00175073
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51984 +/- 8.91544e-06
  return (TDataSet *)tableSet;// West = 5.51916 +/- 9.706e-06 East = 5.52353 +/- 2.25533e-05
};
