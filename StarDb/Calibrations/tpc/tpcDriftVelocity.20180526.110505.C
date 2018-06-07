TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 146018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55214; // +/- 7.92208e-06 cm/us All: East = -0.247659 +/- 0.00835477
  row.laserDriftVelocityWest	 =   5.55214; // +/- 7.92208e-06 cm/us All: West = 0.0106998 +/- 0.00141095
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55214 +/- 7.92208e-06
  return (TDataSet *)tableSet;// West = 5.55209 +/- 8.07229e-06 East = 5.55341 +/- 4.12573e-05
};
