TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 156043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55361; // +/- 9.00737e-06 cm/us All: East = 0.162998 +/- 0.00384145
  row.laserDriftVelocityWest	 =   5.55361; // +/- 9.00737e-06 cm/us All: West = 0.348132 +/- 0.001779
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55361 +/- 9.00737e-06
  return (TDataSet *)tableSet;// West = 5.55343 +/- 9.91209e-06 East = 5.55446 +/- 2.158e-05
};
