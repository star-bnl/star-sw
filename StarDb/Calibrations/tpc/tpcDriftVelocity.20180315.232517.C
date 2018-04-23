TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74101
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53227; // +/- 9.84301e-06 cm/us All: East = -0.221634 +/- 0.0087797
  row.laserDriftVelocityWest	 =   5.53227; // +/- 9.84301e-06 cm/us All: West = 0.170405 +/- 0.00179227
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53227 +/- 9.84301e-06
  return (TDataSet *)tableSet;// West = 5.53218 +/- 1.00412e-05 East = 5.53437 +/- 4.979e-05
};
