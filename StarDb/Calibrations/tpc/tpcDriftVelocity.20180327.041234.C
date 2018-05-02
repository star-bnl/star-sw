TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 86001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52899; // +/- 9.65712e-05 cm/us All: East = -0.275642 +/- 1.35894
  row.laserDriftVelocityWest	 =   5.52899; // +/- 9.65712e-05 cm/us All: West = 0.0205075 +/- 0.0974056
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52899 +/- 9.65712e-05
  return (TDataSet *)tableSet;// West = 5.52902 +/- 9.66573e-05 East = 5.517 +/- 0.00228847
};
