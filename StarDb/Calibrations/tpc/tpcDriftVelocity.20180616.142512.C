TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 167023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54672; // +/- 5.20719e-06 cm/us All: East = 0.303645 +/- 0.0019632
  row.laserDriftVelocityWest	 =   5.54672; // +/- 5.20719e-06 cm/us All: West = 0.168938 +/- 0.00104114
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54672 +/- 5.20719e-06
  return (TDataSet *)tableSet;// West = 5.54687 +/- 5.90274e-06 East = 5.54616 +/- 1.1057e-05
};
