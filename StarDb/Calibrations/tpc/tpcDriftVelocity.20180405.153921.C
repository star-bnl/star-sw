TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5233; // +/- 1.09325e-05 cm/us All: East = -0.032475 +/- 0.00270422
  row.laserDriftVelocityWest	 =   5.5233; // +/- 1.09325e-05 cm/us All: West = 0.333492 +/- 0.00280263
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5233 +/- 1.09325e-05
  return (TDataSet *)tableSet;// West = 5.52225 +/- 1.56887e-05 East = 5.52429 +/- 1.52428e-05
};
