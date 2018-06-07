TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 150006
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54968; // +/- 6.46274e-06 cm/us All: East = 0.207548 +/- 0.00339859
  row.laserDriftVelocityWest	 =   5.54968; // +/- 6.46274e-06 cm/us All: West = 0.26422 +/- 0.00122517
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54968 +/- 6.46274e-06
  return (TDataSet *)tableSet;// West = 5.54964 +/- 6.89522e-06 East = 5.54996 +/- 1.854e-05
};
