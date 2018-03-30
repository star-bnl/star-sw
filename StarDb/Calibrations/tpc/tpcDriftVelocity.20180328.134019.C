TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 87050
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51823; // +/- 5.37264e-06 cm/us All: East = 3.68463 +/- 0.00300311
  row.laserDriftVelocityWest	 =   5.51823; // +/- 5.37264e-06 cm/us All: West = 4.37719 +/- 0.0010256
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51823 +/- 5.37264e-06
  return (TDataSet *)tableSet;// West = 5.51782 +/- 5.68009e-06 East = 5.5217 +/- 1.65547e-05
};
