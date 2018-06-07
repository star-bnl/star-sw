TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 148051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55144; // +/- 5.19205e-06 cm/us All: East = -0.353241 +/- 0.00310459
  row.laserDriftVelocityWest	 =   5.55144; // +/- 5.19205e-06 cm/us All: West = 0.127762 +/- 0.000966274
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55144 +/- 5.19205e-06
  return (TDataSet *)tableSet;// West = 5.55121 +/- 5.43085e-06 East = 5.55386 +/- 1.77039e-05
};
