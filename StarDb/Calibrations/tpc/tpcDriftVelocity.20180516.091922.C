TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 136012
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53776; // +/- 5.17522e-06 cm/us All: East = -0.111996 +/- 0.00388669
  row.laserDriftVelocityWest	 =   5.53776; // +/- 5.17522e-06 cm/us All: West = 0.129585 +/- 0.000949064
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53776 +/- 5.17522e-06
  return (TDataSet *)tableSet;// West = 5.53768 +/- 5.32123e-06 East = 5.53905 +/- 2.22446e-05
};
