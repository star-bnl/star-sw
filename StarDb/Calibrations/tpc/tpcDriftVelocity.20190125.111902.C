TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 25020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5586; // +/- 1.15809e-05 cm/us All: East = -0.483227 +/- 0.00347539
  row.laserDriftVelocityWest	 =   5.5586; // +/- 1.15809e-05 cm/us All: West = 0.0630573 +/- 0.00257235
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5586 +/- 1.15809e-05
  return (TDataSet *)tableSet;// West = 5.55758 +/- 1.42995e-05 East = 5.56055 +/- 1.97427e-05
};
