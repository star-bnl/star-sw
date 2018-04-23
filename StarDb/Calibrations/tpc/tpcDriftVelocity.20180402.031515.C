TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 91119
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52873; // +/- 1.39311e-05 cm/us All: East = -0.495864 +/- 0.00655266
  row.laserDriftVelocityWest	 =   5.52873; // +/- 1.39311e-05 cm/us All: West = 0.352311 +/- 0.00272744
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52873 +/- 1.39311e-05
  return (TDataSet *)tableSet;// West = 5.52807 +/- 1.5043e-05 East = 5.53274 +/- 3.6922e-05
};
