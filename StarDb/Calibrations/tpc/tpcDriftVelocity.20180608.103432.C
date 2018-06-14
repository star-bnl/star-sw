TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 159015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55036; // +/- 5.08248e-06 cm/us All: East = 0.119411 +/- 0.00230753
  row.laserDriftVelocityWest	 =   5.55036; // +/- 5.08248e-06 cm/us All: West = 0.182158 +/- 0.000972255
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55036 +/- 5.08248e-06
  return (TDataSet *)tableSet;// West = 5.55031 +/- 5.51737e-06 East = 5.55065 +/- 1.30607e-05
};
