TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 55055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.50392; // +/- 1.36099e-05 cm/us All: East = -0.269369 +/- 0.00410508
  row.laserDriftVelocityWest	 =   5.50392; // +/- 1.36099e-05 cm/us All: West = 0.325768 +/- 0.00302151
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.50392 +/- 1.36099e-05
  return (TDataSet *)tableSet;// West = 5.50296 +/- 1.65476e-05 East = 5.50592 +/- 2.39267e-05
};
