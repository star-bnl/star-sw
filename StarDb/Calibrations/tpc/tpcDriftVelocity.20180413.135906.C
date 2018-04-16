TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55505; // +/- 1.85733e-05 cm/us All: East = -5.99504 +/- 0.00445405
  row.laserDriftVelocityWest	 =   5.55505; // +/- 1.85733e-05 cm/us All: West = -5.61638 +/- 0.00511266
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55505 +/- 1.85733e-05
  return (TDataSet *)tableSet;// West = 5.55385 +/- 2.89526e-05 East = 5.55588 +/- 2.42117e-05
};
