TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 131061
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54843; // +/- 5.51414e-06 cm/us All: East = 0.110113 +/- 0.00174509
  row.laserDriftVelocityWest	 =   5.54843; // +/- 5.51414e-06 cm/us All: West = 0.202016 +/- 0.00118894
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54843 +/- 5.51414e-06
  return (TDataSet *)tableSet;// West = 5.54827 +/- 6.67851e-06 East = 5.54879 +/- 9.77377e-06
};
