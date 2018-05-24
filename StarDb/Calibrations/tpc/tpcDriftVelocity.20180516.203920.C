TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 136033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53806; // +/- 4.62289e-06 cm/us All: East = 0.186612 +/- 0.00174416
  row.laserDriftVelocityWest	 =   5.53806; // +/- 4.62289e-06 cm/us All: West = 0.148269 +/- 0.000926431
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53806 +/- 4.62289e-06
  return (TDataSet *)tableSet;// West = 5.5381 +/- 5.26663e-06 East = 5.53792 +/- 9.64941e-06
};
