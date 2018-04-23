TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55508; // +/- 1.78449e-05 cm/us All: East = 0.0229656 +/- 0.00425893
  row.laserDriftVelocityWest	 =   5.55508; // +/- 1.78449e-05 cm/us All: West = 0.406711 +/- 0.00527412
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55508 +/- 1.78449e-05
  return (TDataSet *)tableSet;// West = 5.55386 +/- 2.86601e-05 East = 5.55585 +/- 2.28047e-05
};
