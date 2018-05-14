TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 134008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54768; // +/- 1.19754e-05 cm/us All: East = -0.0820644 +/- 0.00403509
  row.laserDriftVelocityWest	 =   5.54768; // +/- 1.19754e-05 cm/us All: West = 0.48801 +/- 0.00252927
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54768 +/- 1.19754e-05
  return (TDataSet *)tableSet;// West = 5.54683 +/- 1.41197e-05 East = 5.54988 +/- 2.26044e-05
};
