TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 116007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54363; // +/- 5.69577e-06 cm/us All: East = -0.573513 +/- 0.0137861
  row.laserDriftVelocityWest	 =   5.54363; // +/- 5.69577e-06 cm/us All: West = 0.173734 +/- 0.00101141
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54363 +/- 5.69577e-06
  return (TDataSet *)tableSet;// West = 5.5436 +/- 5.71472e-06 East = 5.54783 +/- 6.99989e-05
};
