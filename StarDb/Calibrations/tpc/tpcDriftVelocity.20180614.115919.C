TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 165021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54798; // +/- 6.70565e-06 cm/us All: East = 0.000867588 +/- 0.00424146
  row.laserDriftVelocityWest	 =   5.54798; // +/- 6.70565e-06 cm/us All: West = 0.22085 +/- 0.00124067
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54798 +/- 6.70565e-06
  return (TDataSet *)tableSet;// West = 5.54788 +/- 7.00889e-06 East = 5.54901 +/- 2.30465e-05
};
