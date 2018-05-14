TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 132044
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54772; // +/- 7.39071e-06 cm/us All: East = 0.0770547 +/- 0.00264289
  row.laserDriftVelocityWest	 =   5.54772; // +/- 7.39071e-06 cm/us All: West = 0.308509 +/- 0.00150478
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54772 +/- 7.39071e-06
  return (TDataSet *)tableSet;// West = 5.54742 +/- 8.47371e-06 East = 5.54869 +/- 1.51089e-05
};
