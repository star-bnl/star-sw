TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 19064
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55861; // +/- 1.59733e-05 cm/us All: East = 0.165009 +/- 0.00278246
  row.laserDriftVelocityWest	 =   5.55861; // +/- 1.59733e-05 cm/us All: West = -999 +/- 999
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55861 +/- 1.59733e-05
  return (TDataSet *)tableSet;// West = -999 +/- 999 East = 5.55861 +/- 1.59733e-05
};
