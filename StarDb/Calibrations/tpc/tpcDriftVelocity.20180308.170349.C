TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 67033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5302; // +/- 1.40756e-05 cm/us All: East = 0.121314 +/- 0.00520156
  row.laserDriftVelocityWest	 =   5.5302; // +/- 1.40756e-05 cm/us All: West = 0.260776 +/- 0.00280007
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5302 +/- 1.40756e-05
  return (TDataSet *)tableSet;// West = 5.52978 +/- 1.56363e-05 East = 5.53201 +/- 3.23204e-05
};
