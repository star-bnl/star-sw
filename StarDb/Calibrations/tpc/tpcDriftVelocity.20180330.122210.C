TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51677; // +/- 1.77557e-05 cm/us All: East = -0.769716 +/- 0.00855867
  row.laserDriftVelocityWest	 =   5.51677; // +/- 1.77557e-05 cm/us All: West = 0.237957 +/- 0.00345419
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51677 +/- 1.77557e-05
  return (TDataSet *)tableSet;// West = 5.51596 +/- 1.9218e-05 East = 5.52151 +/- 4.64072e-05
};
