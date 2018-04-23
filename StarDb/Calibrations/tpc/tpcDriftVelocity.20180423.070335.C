TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 113007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55097; // +/- 7.78139e-06 cm/us All: East = 0.389682 +/- 0.00303068
  row.laserDriftVelocityWest	 =   5.55097; // +/- 7.78139e-06 cm/us All: West = 0.81795 +/- 0.00155999
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55097 +/- 7.78139e-06
  return (TDataSet *)tableSet;// West = 5.55051 +/- 8.71449e-06 East = 5.55278 +/- 1.72841e-05
};
