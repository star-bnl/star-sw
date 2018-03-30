TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 86008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52904; // +/- 7.87003e-06 cm/us All: East = 1.6552 +/- 0.00668751
  row.laserDriftVelocityWest	 =   5.52904; // +/- 7.87003e-06 cm/us All: West = 2.37391 +/- 0.00145125
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52904 +/- 7.87003e-06
  return (TDataSet *)tableSet;// West = 5.52888 +/- 8.0465e-06 East = 5.53256 +/- 3.77854e-05
};
