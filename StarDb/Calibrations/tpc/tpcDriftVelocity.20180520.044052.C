TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 140003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55879; // +/- 5.73799e-06 cm/us All: East = 0.293281 +/- 0.00519812
  row.laserDriftVelocityWest	 =   5.55879; // +/- 5.73799e-06 cm/us All: West = 0.190782 +/- 0.00104018
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55879 +/- 5.73799e-06
  return (TDataSet *)tableSet;// West = 5.55882 +/- 5.85825e-06 East = 5.55817 +/- 2.84657e-05
};
