TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54442; // +/- 2.2985e-05 cm/us All: East = -0.0172564 +/- 1.33097
  row.laserDriftVelocityWest	 =   5.54442; // +/- 2.2985e-05 cm/us All: West = 0.176871 +/- 0.00419281
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54442 +/- 2.2985e-05
  return (TDataSet *)tableSet;// West = 5.54442 +/- 2.2985e-05 East = 5.62176 +/- 0.0812435
};
