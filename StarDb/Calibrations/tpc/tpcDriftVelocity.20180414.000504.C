TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54412; // +/- 8.48607e-06 cm/us All: East = -0.118966 +/- 0.00189664
  row.laserDriftVelocityWest	 =   5.54412; // +/- 8.48607e-06 cm/us All: West = 0.156975 +/- 0.00251773
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54412 +/- 8.48607e-06
  return (TDataSet *)tableSet;// West = 5.54315 +/- 1.41465e-05 East = 5.54466 +/- 1.06063e-05
};
