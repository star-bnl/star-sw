TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 152040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55063; // +/- 4.95162e-06 cm/us All: East = 0.246994 +/- 0.00178566
  row.laserDriftVelocityWest	 =   5.55063; // +/- 4.95162e-06 cm/us All: West = 0.225266 +/- 0.000989955
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55063 +/- 4.95162e-06
  return (TDataSet *)tableSet;// West = 5.55066 +/- 5.67974e-06 East = 5.55052 +/- 1.01083e-05
};
