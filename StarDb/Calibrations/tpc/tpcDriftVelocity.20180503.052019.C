TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 123003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53097; // +/- 8.52682e-06 cm/us All: East = -0.544261 +/- 0.00335714
  row.laserDriftVelocityWest	 =   5.53097; // +/- 8.52682e-06 cm/us All: West = 0.271502 +/- 0.00172853
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53097 +/- 8.52682e-06
  return (TDataSet *)tableSet;// West = 5.53003 +/- 9.59994e-06 East = 5.53447 +/- 1.85597e-05
};
