TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55402; // +/- 0.000115165 cm/us All: East = 0.00674176 +/- 0.0353893
  row.laserDriftVelocityWest	 =   5.55402; // +/- 0.000115165 cm/us All: West = 1.861 +/- 0.609229
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55402 +/- 0.000115165
  return (TDataSet *)tableSet;// West = 5.54565 +/- 0.000326291 East = 5.55521 +/- 0.000123086
};
