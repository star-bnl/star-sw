TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 151048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55014; // +/- 6.39746e-06 cm/us All: East = -0.400989 +/- 0.00241656
  row.laserDriftVelocityWest	 =   5.55014; // +/- 6.39746e-06 cm/us All: West = 0.225943 +/- 0.00128053
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55014 +/- 6.39746e-06
  return (TDataSet *)tableSet;// West = 5.5494 +/- 7.20732e-06 East = 5.55288 +/- 1.38909e-05
};
