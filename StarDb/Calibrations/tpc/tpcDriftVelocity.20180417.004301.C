TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106049
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5449; // +/- 1.78355e-05 cm/us All: East = -0.316998 +/- 0.565846
  row.laserDriftVelocityWest	 =   5.5449; // +/- 1.78355e-05 cm/us All: West = 0.132856 +/- 0.00324955
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5449 +/- 1.78355e-05
  return (TDataSet *)tableSet;// West = 5.54484 +/- 1.79445e-05 East = 5.54955 +/- 0.000162017
};
