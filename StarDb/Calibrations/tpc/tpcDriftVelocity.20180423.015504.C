TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55154; // +/- 9.69613e-06 cm/us All: East = 0.293774 +/- 0.00285107
  row.laserDriftVelocityWest	 =   5.55154; // +/- 9.69613e-06 cm/us All: West = 0.735254 +/- 0.00152375
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55154 +/- 9.69613e-06
  return (TDataSet *)tableSet;// West = 5.55044 +/- 1.23328e-05 East = 5.55332 +/- 1.56905e-05
};
