TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54343; // +/- 7.58522e-06 cm/us All: East = -0.101548 +/- 0.00350852
  row.laserDriftVelocityWest	 =   5.54343; // +/- 7.58522e-06 cm/us All: West = 0.221857 +/- 0.00146581
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54343 +/- 7.58522e-06
  return (TDataSet *)tableSet;// West = 5.54318 +/- 8.2255e-06 East = 5.54483 +/- 1.96097e-05
};
