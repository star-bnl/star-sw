TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53004; // +/- 1.27223e-05 cm/us All: East = 1.64587 +/- 0.00922262
  row.laserDriftVelocityWest	 =   5.53004; // +/- 1.27223e-05 cm/us All: West = 2.19493 +/- 0.00234037
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53004 +/- 1.27223e-05
  return (TDataSet *)tableSet;// West = 5.52983 +/- 1.31763e-05 East = 5.53289 +/- 4.88837e-05
};
