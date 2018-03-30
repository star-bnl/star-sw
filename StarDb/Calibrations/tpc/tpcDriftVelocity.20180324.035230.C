TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 82047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53493; // +/- 9.54419e-06 cm/us All: East = 0.619988 +/- 0.0069041
  row.laserDriftVelocityWest	 =   5.53493; // +/- 9.54419e-06 cm/us All: West = 1.31759 +/- 0.00176432
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53493 +/- 9.54419e-06
  return (TDataSet *)tableSet;// West = 5.53469 +/- 9.85509e-06 East = 5.53852 +/- 3.83e-05
};
