TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 16061
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55812; // +/- 1.68527e-05 cm/us All: East = -0.0391952 +/- 0.00492588
  row.laserDriftVelocityWest	 =   5.55812; // +/- 1.68527e-05 cm/us All: West = 0.348789 +/- 0.00392648
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55812 +/- 1.68527e-05
  return (TDataSet *)tableSet;// West = 5.55728 +/- 2.14171e-05 East = 5.55948 +/- 2.73095e-05
};
