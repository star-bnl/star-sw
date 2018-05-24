TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 138034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55036; // +/- 7.45665e-06 cm/us All: East = 0.187158 +/- 0.00189451
  row.laserDriftVelocityWest	 =   5.55036; // +/- 7.45665e-06 cm/us All: West = 0.174484 +/- 0.00106865
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55036 +/- 7.45665e-06
  return (TDataSet *)tableSet;// West = 5.54987 +/- 1.04896e-05 East = 5.55085 +/- 1.06019e-05
};
