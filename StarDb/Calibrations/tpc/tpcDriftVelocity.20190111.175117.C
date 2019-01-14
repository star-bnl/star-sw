TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 11004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52197; // +/- 2.09158e-05 cm/us All: East = 4.18588 +/- 0.00708517
  row.laserDriftVelocityWest	 =   5.52197; // +/- 2.09158e-05 cm/us All: West = 4.72559 +/- 0.00494137
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52197 +/- 2.09158e-05
  return (TDataSet *)tableSet;// West = 5.52091 +/- 2.59544e-05 East = 5.52394 +/- 3.5325e-05
};
