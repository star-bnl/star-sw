TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 155046
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55775; // +/- 1.12812e-05 cm/us All: East = -0.371663 +/- 0.00234525
  row.laserDriftVelocityWest	 =   5.55775; // +/- 1.12812e-05 cm/us All: West = 0.217477 +/- 0.00347486
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55775 +/- 1.12812e-05
  return (TDataSet *)tableSet;// West = 5.55526 +/- 2.14956e-05 East = 5.5587 +/- 1.3253e-05
};
