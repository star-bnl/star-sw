TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 121029
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53192; // +/- 6.824e-06 cm/us All: East = 0.104455 +/- 0.00251463
  row.laserDriftVelocityWest	 =   5.53192; // +/- 6.824e-06 cm/us All: West = 0.340915 +/- 0.00141752
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53192 +/- 6.824e-06
  return (TDataSet *)tableSet;// West = 5.53159 +/- 7.8663e-06 East = 5.53291 +/- 1.37182e-05
};
