TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 77035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55761; // +/- 1.56939e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.55761; // +/- 1.56939e-05 cm/us All: West = 0.198384 +/- 0.00278427
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55761 +/- 1.56939e-05
  return (TDataSet *)tableSet;// West = 5.55761 +/- 1.56939e-05 East = -999 +/- 999
};
