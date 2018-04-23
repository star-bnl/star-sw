TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93030
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53021; // +/- 9.60611e-06 cm/us All: East = -0.720923 +/- 0.00632279
  row.laserDriftVelocityWest	 =   5.53021; // +/- 9.60611e-06 cm/us All: West = 0.258101 +/- 0.00178716
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53021 +/- 9.60611e-06
  return (TDataSet *)tableSet;// West = 5.52982 +/- 9.97114e-06 East = 5.53524 +/- 3.58304e-05
};
