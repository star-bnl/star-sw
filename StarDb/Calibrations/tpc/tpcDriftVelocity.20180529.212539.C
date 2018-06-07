TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 149043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5502; // +/- 5.64658e-06 cm/us All: East = -0.302508 +/- 0.0059516
  row.laserDriftVelocityWest	 =   5.5502; // +/- 5.64658e-06 cm/us All: West = 0.179349 +/- 0.00100602
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5502 +/- 5.64658e-06
  return (TDataSet *)tableSet;// West = 5.55015 +/- 5.70489e-06 East = 5.55254 +/- 3.95965e-05
};
