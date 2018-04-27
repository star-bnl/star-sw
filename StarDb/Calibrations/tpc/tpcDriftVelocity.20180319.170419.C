TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 78015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55363; // +/- 0.000250556 cm/us All: East = 3.48007 +/- 28.9775
  row.laserDriftVelocityWest	 =   5.55363; // +/- 0.000250556 cm/us All: West = 1.93787 +/- 0.158698
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55363 +/- 0.000250556
  return (TDataSet *)tableSet;// West = 5.55363 +/- 0.000250556 East = 5.47572 +/- 0.290123
};
