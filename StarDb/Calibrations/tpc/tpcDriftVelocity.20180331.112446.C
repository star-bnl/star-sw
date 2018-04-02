TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52725; // +/- 0.000100158 cm/us All: East = -3.39239 +/- 2.46011
  row.laserDriftVelocityWest	 =   5.52725; // +/- 0.000100158 cm/us All: West = -2.05355 +/- 0.135319
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52725 +/- 0.000100158
  return (TDataSet *)tableSet;// West = 5.52724 +/- 0.000100212 East = 5.53822 +/- 0.00305208
};
