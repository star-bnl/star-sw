TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53391; // +/- 2.71572e-05 cm/us All: East = -0.0647353 +/- 0.00493611
  row.laserDriftVelocityWest	 =   5.53391; // +/- 2.71572e-05 cm/us All: West = -999 +/- 999
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53391 +/- 2.71572e-05
  return (TDataSet *)tableSet;// West = -999 +/- 999 East = 5.53391 +/- 2.71572e-05
};
