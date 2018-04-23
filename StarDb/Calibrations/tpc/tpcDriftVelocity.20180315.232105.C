TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74100
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53228; // +/- 1.46441e-05 cm/us All: East = 0.234647 +/- 0.0147106
  row.laserDriftVelocityWest	 =   5.53228; // +/- 1.46441e-05 cm/us All: West = 0.138183 +/- 0.00271528
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53228 +/- 1.46441e-05
  return (TDataSet *)tableSet;// West = 5.53229 +/- 1.49677e-05 East = 5.53189 +/- 7.08137e-05
};
