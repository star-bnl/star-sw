TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 147026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5501; // +/- 4.32263e-06 cm/us All: East = 0.155108 +/- 0.00151028
  row.laserDriftVelocityWest	 =   5.5501; // +/- 4.32263e-06 cm/us All: West = 0.188057 +/- 0.000864828
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5501 +/- 4.32263e-06
  return (TDataSet *)tableSet;// West = 5.55009 +/- 5.00506e-06 East = 5.55013 +/- 8.57512e-06
};
