TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103032
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54501; // +/- 1.88626e-05 cm/us All: East = -0.308947 +/- 0.00403754
  row.laserDriftVelocityWest	 =   5.54501; // +/- 1.88626e-05 cm/us All: West = 0.0506305 +/- 0.0069906
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54501 +/- 1.88626e-05
  return (TDataSet *)tableSet;// West = 5.54365 +/- 3.58485e-05 East = 5.54553 +/- 2.21815e-05
};
