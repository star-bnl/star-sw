TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 160015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54945; // +/- 6.12146e-06 cm/us All: East = 0.086558 +/- 0.00276494
  row.laserDriftVelocityWest	 =   5.54945; // +/- 6.12146e-06 cm/us All: West = 0.165321 +/- 0.00118154
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54945 +/- 6.12146e-06
  return (TDataSet *)tableSet;// West = 5.54939 +/- 6.68357e-06 East = 5.54979 +/- 1.52498e-05
};
