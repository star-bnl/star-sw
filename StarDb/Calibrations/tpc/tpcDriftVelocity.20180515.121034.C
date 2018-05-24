TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 135021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54571; // +/- 4.71477e-06 cm/us All: East = 0.34135 +/- 0.0017268
  row.laserDriftVelocityWest	 =   5.54571; // +/- 4.71477e-06 cm/us All: West = 0.213022 +/- 0.000954811
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54571 +/- 4.71477e-06
  return (TDataSet *)tableSet;// West = 5.54587 +/- 5.39368e-06 East = 5.5452 +/- 9.70723e-06
};
