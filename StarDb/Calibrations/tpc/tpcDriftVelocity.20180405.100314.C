TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52196; // +/- 4.34411e-05 cm/us All: East = -0.392572 +/- 0.0497922
  row.laserDriftVelocityWest	 =   5.52196; // +/- 4.34411e-05 cm/us All: West = 0.25723 +/- 0.0139995
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52196 +/- 4.34411e-05
  return (TDataSet *)tableSet;// West = 5.5214 +/- 4.75268e-05 East = 5.52477 +/- 0.000107093
};
