TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 102052
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55393; // +/- 1.84956e-05 cm/us All: East = -6.26693 +/- 0.693287
  row.laserDriftVelocityWest	 =   5.55393; // +/- 1.84956e-05 cm/us All: West = -5.61514 +/- 0.00331932
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55393 +/- 1.84956e-05
  return (TDataSet *)tableSet;// West = 5.55381 +/- 1.86904e-05 East = 5.56002 +/- 0.000128434
};
