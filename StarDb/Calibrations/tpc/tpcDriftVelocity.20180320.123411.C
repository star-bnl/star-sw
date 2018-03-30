TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56569; // +/- 7.70712e-06 cm/us All: East = -4.30332 +/- 0.00600147
  row.laserDriftVelocityWest	 =   5.56569; // +/- 7.70712e-06 cm/us All: West = -4.25513 +/- 0.00139977
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56569 +/- 7.70712e-06
  return (TDataSet *)tableSet;// West = 5.56567 +/- 7.92475e-06 East = 5.56608 +/- 3.3114e-05
};
