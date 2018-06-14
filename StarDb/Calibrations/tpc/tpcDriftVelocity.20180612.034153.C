TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162044
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54772; // +/- 9.30268e-06 cm/us All: East = -0.480969 +/- 0.00314282
  row.laserDriftVelocityWest	 =   5.54772; // +/- 9.30268e-06 cm/us All: West = 0.223181 +/- 0.00195829
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54772 +/- 9.30268e-06
  return (TDataSet *)tableSet;// West = 5.54669 +/- 1.08941e-05 East = 5.55051 +/- 1.78758e-05
};
