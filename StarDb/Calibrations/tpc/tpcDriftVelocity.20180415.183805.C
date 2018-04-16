TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55351; // +/- 9.10683e-06 cm/us All: East = -5.68534 +/- 0.00225244
  row.laserDriftVelocityWest	 =   5.55351; // +/- 9.10683e-06 cm/us All: West = -5.43806 +/- 0.00231118
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55351 +/- 9.10683e-06
  return (TDataSet *)tableSet;// West = 5.55281 +/- 1.30533e-05 East = 5.55417 +/- 1.27115e-05
};
