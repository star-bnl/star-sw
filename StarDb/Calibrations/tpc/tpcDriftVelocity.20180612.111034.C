TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 163015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54802; // +/- 1.08616e-05 cm/us All: East = -0.778436 +/- 0.00526803
  row.laserDriftVelocityWest	 =   5.54802; // +/- 1.08616e-05 cm/us All: West = 0.24985 +/- 0.00209191
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54802 +/- 1.08616e-05
  return (TDataSet *)tableSet;// West = 5.54723 +/- 1.17018e-05 East = 5.55296 +/- 2.91908e-05
};
