TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51853; // +/- 1.16183e-05 cm/us All: East = -0.119229 +/- 0.00634585
  row.laserDriftVelocityWest	 =   5.51853; // +/- 1.16183e-05 cm/us All: West = 0.2214 +/- 0.00227157
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51853 +/- 1.16183e-05
  return (TDataSet *)tableSet;// West = 5.51836 +/- 1.24193e-05 East = 5.51972 +/- 3.28837e-05
};
