TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54143; // +/- 7.46399e-06 cm/us All: East = -0.138711 +/- 0.00277465
  row.laserDriftVelocityWest	 =   5.54143; // +/- 7.46399e-06 cm/us All: West = 0.245346 +/- 0.00152203
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54143 +/- 7.46399e-06
  return (TDataSet *)tableSet;// West = 5.54097 +/- 8.52195e-06 East = 5.54295 +/- 1.5467e-05
};
