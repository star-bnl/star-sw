TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55391; // +/- 8.31605e-06 cm/us All: East = -5.72885 +/- 0.00200864
  row.laserDriftVelocityWest	 =   5.55391; // +/- 8.31605e-06 cm/us All: West = -5.52468 +/- 0.00212313
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55391 +/- 8.31605e-06
  return (TDataSet *)tableSet;// West = 5.55331 +/- 1.20437e-05 East = 5.55445 +/- 1.14967e-05
};
