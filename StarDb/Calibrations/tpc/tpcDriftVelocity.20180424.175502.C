TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 114031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54101; // +/- 8.94984e-06 cm/us All: East = 0.132177 +/- 0.00464263
  row.laserDriftVelocityWest	 =   5.54101; // +/- 8.94984e-06 cm/us All: West = 0.177815 +/- 0.00173983
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54101 +/- 8.94984e-06
  return (TDataSet *)tableSet;// West = 5.541 +/- 9.61105e-06 East = 5.54111 +/- 2.45537e-05
};
