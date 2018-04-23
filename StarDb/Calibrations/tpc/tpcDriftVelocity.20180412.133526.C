TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 102034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5434; // +/- 1.69546e-05 cm/us All: East = 0.102234 +/- 0.00453615
  row.laserDriftVelocityWest	 =   5.5434; // +/- 1.69546e-05 cm/us All: West = 0.28929 +/- 0.00440116
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5434 +/- 1.69546e-05
  return (TDataSet *)tableSet;// West = 5.54294 +/- 2.33371e-05 East = 5.54392 +/- 2.46736e-05
};
