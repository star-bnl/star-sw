TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79012
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56573; // +/- 7.31751e-06 cm/us All: East = 0.0804535 +/- 0.00586524
  row.laserDriftVelocityWest	 =   5.56573; // +/- 7.31751e-06 cm/us All: West = 0.164726 +/- 0.00132966
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56573 +/- 7.31751e-06
  return (TDataSet *)tableSet;// West = 5.5657 +/- 7.51896e-06 East = 5.56631 +/- 3.18257e-05
};
