TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 153019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.552; // +/- 5.74103e-06 cm/us All: East = 0.123665 +/- 0.00220334
  row.laserDriftVelocityWest	 =   5.552; // +/- 5.74103e-06 cm/us All: West = 0.0264464 +/- 0.00115428
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.552 +/- 5.74103e-06
  return (TDataSet *)tableSet;// West = 5.55212 +/- 6.48407e-06 East = 5.55157 +/- 1.23511e-05
};
