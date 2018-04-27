TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54258; // +/- 1.17697e-05 cm/us All: East = -0.435602 +/- 0.655702
  row.laserDriftVelocityWest	 =   5.54258; // +/- 1.17697e-05 cm/us All: West = 0.172779 +/- 0.00212729
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54258 +/- 1.17697e-05
  return (TDataSet *)tableSet;// West = 5.54254 +/- 1.18211e-05 East = 5.54672 +/- 0.000126293
};
