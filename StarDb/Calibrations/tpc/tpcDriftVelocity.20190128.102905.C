TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 28014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52762; // +/- 8.64189e-06 cm/us All: East = 0.985998 +/- 0.00410993
  row.laserDriftVelocityWest	 =   5.52762; // +/- 8.64189e-06 cm/us All: West = 1.44722 +/- 0.00165481
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52762 +/- 8.64189e-06
  return (TDataSet *)tableSet;// West = 5.52733 +/- 9.27373e-06 East = 5.52953 +/- 2.38202e-05
};
