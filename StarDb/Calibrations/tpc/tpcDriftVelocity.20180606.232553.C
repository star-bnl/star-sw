TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 157035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55239; // +/- 4.8294e-06 cm/us All: East = 0.157904 +/- 0.00147719
  row.laserDriftVelocityWest	 =   5.55239; // +/- 4.8294e-06 cm/us All: West = 0.158727 +/- 0.00104835
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55239 +/- 4.8294e-06
  return (TDataSet *)tableSet;// West = 5.55238 +/- 5.96198e-06 East = 5.5524 +/- 8.2359e-06
};
