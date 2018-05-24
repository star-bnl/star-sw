TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 134028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54669; // +/- 9.35705e-06 cm/us All: East = -0.171576 +/- 0.00918918
  row.laserDriftVelocityWest	 =   5.54669; // +/- 9.35705e-06 cm/us All: West = 0.200595 +/- 0.0017114
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54669 +/- 9.35705e-06
  return (TDataSet *)tableSet;// West = 5.54665 +/- 9.52855e-06 East = 5.54776 +/- 4.95418e-05
};
