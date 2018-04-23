TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55305; // +/- 8.10213e-06 cm/us All: East = 0.0955228 +/- 0.00193029
  row.laserDriftVelocityWest	 =   5.55305; // +/- 8.10213e-06 cm/us All: West = 0.277553 +/- 0.00217213
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55305 +/- 8.10213e-06
  return (TDataSet *)tableSet;// West = 5.55248 +/- 1.22125e-05 East = 5.5535 +/- 1.08283e-05
};
