TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55175; // +/- 8.6003e-06 cm/us All: East = -5.5622 +/- 0.00245617
  row.laserDriftVelocityWest	 =   5.55175; // +/- 8.6003e-06 cm/us All: West = -5.03913 +/- 0.00190012
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55175 +/- 8.6003e-06
  return (TDataSet *)tableSet;// West = 5.55056 +/- 1.07773e-05 East = 5.55383 +/- 1.42707e-05
};
