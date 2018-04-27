TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54245; // +/- 2.05669e-05 cm/us All: East = 0.0347377 +/- 0.00454612
  row.laserDriftVelocityWest	 =   5.54245; // +/- 2.05669e-05 cm/us All: West = 0.664584 +/- 0.00754335
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54245 +/- 2.05669e-05
  return (TDataSet *)tableSet;// West = 5.53978 +/- 3.93812e-05 East = 5.54345 +/- 2.41171e-05
};
