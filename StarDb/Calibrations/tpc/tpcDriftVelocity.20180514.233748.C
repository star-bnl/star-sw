TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 134041
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54642; // +/- 5.02475e-06 cm/us All: East = 0.259313 +/- 0.00201627
  row.laserDriftVelocityWest	 =   5.54642; // +/- 5.02475e-06 cm/us All: West = 0.15964 +/- 0.000986872
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54642 +/- 5.02475e-06
  return (TDataSet *)tableSet;// West = 5.54652 +/- 5.59434e-06 East = 5.54598 +/- 1.14298e-05
};
