TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 144037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55063; // +/- 0.00011634 cm/us All: East = -0.439667 +/- 0.830569
  row.laserDriftVelocityWest	 =   5.55063; // +/- 0.00011634 cm/us All: West = 0.31865 +/- 0.1311
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55063 +/- 0.00011634
  return (TDataSet *)tableSet;// West = 5.55048 +/- 0.000118979 East = 5.55385 +/- 0.000555421
};
