TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80065
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54807; // +/- 1.11911e-05 cm/us All: East = -0.469651 +/- 0.00680117
  row.laserDriftVelocityWest	 =   5.54807; // +/- 1.11911e-05 cm/us All: West = 0.265279 +/- 0.00209984
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54807 +/- 1.11911e-05
  return (TDataSet *)tableSet;// West = 5.54772 +/- 1.1739e-05 East = 5.55161 +/- 3.7064e-05
};
