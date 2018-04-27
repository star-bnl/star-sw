TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 113007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54096; // +/- 7.68568e-06 cm/us All: East = -0.105398 +/- 0.00291746
  row.laserDriftVelocityWest	 =   5.54096; // +/- 7.68568e-06 cm/us All: West = 0.268614 +/- 0.00156185
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54096 +/- 7.68568e-06
  return (TDataSet *)tableSet;// West = 5.54051 +/- 8.76244e-06 East = 5.54249 +/- 1.60026e-05
};
