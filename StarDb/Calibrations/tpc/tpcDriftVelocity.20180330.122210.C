TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52677; // +/- 1.83914e-05 cm/us All: East = -0.742762 +/- 0.00910076
  row.laserDriftVelocityWest	 =   5.52677; // +/- 1.83914e-05 cm/us All: West = 0.302687 +/- 0.0036039
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52677 +/- 1.83914e-05
  return (TDataSet *)tableSet;// West = 5.52591 +/- 1.99347e-05 East = 5.53172 +/- 4.76704e-05
};
