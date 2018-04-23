TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52424; // +/- 7.85228e-06 cm/us All: East = -0.103367 +/- 0.00559033
  row.laserDriftVelocityWest	 =   5.52424; // +/- 7.85228e-06 cm/us All: West = 0.170753 +/- 0.00144953
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52424 +/- 7.85228e-06
  return (TDataSet *)tableSet;// West = 5.52414 +/- 8.11364e-06 East = 5.52572 +/- 3.11889e-05
};
