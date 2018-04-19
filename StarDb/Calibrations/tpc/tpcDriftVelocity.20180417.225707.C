TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55448; // +/- 2.30979e-05 cm/us All: East = 4.14909 +/- 13.854
  row.laserDriftVelocityWest	 =   5.55448; // +/- 2.30979e-05 cm/us All: West = 0.392337 +/- 0.00422867
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55448 +/- 2.30979e-05
  return (TDataSet *)tableSet;// West = 5.55448 +/- 2.30981e-05 East = 5.54966 +/- 0.00562556
};
