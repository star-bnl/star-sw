TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98059
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.541; // +/- 5.85675e-05 cm/us All: East = 0.124622 +/- 0.0363991
  row.laserDriftVelocityWest	 =   5.541; // +/- 5.85675e-05 cm/us All: West = 0.326801 +/- 0.049597
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.541 +/- 5.85675e-05
  return (TDataSet *)tableSet;// West = 5.53996 +/- 0.0001099 East = 5.54142 +/- 6.9215e-05
};
