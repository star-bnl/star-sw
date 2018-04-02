TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51534; // +/- 1.94698e-05 cm/us All: East = 3.71976 +/- 0.0218172
  row.laserDriftVelocityWest	 =   5.51534; // +/- 1.94698e-05 cm/us All: West = 4.87412 +/- 0.00365338
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51534 +/- 1.94698e-05
  return (TDataSet *)tableSet;// West = 5.51511 +/- 1.98583e-05 East = 5.52107 +/- 9.89101e-05
};
