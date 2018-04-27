TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 102044
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54381; // +/- 1.20675e-05 cm/us All: East = -0.899142 +/- 0.00807183
  row.laserDriftVelocityWest	 =   5.54381; // +/- 1.20675e-05 cm/us All: West = 0.223787 +/- 0.00224492
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54381 +/- 1.20675e-05
  return (TDataSet *)tableSet;// West = 5.54341 +/- 1.24759e-05 East = 5.54969 +/- 4.75529e-05
};
