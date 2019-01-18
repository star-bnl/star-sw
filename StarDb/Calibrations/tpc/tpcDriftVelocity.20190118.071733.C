TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 18005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55851; // +/- 1.03685e-05 cm/us All: East = -0.390339 +/- 0.0042115
  row.laserDriftVelocityWest	 =   5.55851; // +/- 1.03685e-05 cm/us All: West = -0.00715741 +/- 0.00202954
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55851 +/- 1.03685e-05
  return (TDataSet *)tableSet;// West = 5.55817 +/- 1.14155e-05 East = 5.56007 +/- 2.47835e-05
};
