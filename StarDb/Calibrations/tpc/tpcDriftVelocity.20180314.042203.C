TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 73004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55101; // +/- 0.000153808 cm/us All: East = 0.264001 +/- 3.86038
  row.laserDriftVelocityWest	 =   5.55101; // +/- 0.000153808 cm/us All: West = 0.13191 +/- 0.10168
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55101 +/- 0.000153808
  return (TDataSet *)tableSet;// West = 5.55101 +/- 0.000153945 East = 5.55282 +/- 0.00365245
};
