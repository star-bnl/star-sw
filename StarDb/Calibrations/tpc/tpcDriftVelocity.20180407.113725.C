TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54533; // +/- 7.82502e-06 cm/us All: East = -4.24786 +/- 0.00213868
  row.laserDriftVelocityWest	 =   5.54533; // +/- 7.82502e-06 cm/us All: West = -3.99106 +/- 0.00183667
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54533 +/- 7.82502e-06
  return (TDataSet *)tableSet;// West = 5.54475 +/- 1.02759e-05 East = 5.54612 +/- 1.20724e-05
};
