TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 91017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52891; // +/- 1.08391e-05 cm/us All: East = -2.98043 +/- 0.00603331
  row.laserDriftVelocityWest	 =   5.52891; // +/- 1.08391e-05 cm/us All: West = -2.18732 +/- 0.00205214
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52891 +/- 1.08391e-05
  return (TDataSet *)tableSet;// West = 5.52845 +/- 1.14614e-05 East = 5.53282 +/- 3.3347e-05
};
