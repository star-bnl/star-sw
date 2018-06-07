TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 147015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54971; // +/- 0.000156657 cm/us All: East = 0.411502 +/- 0.184239
  row.laserDriftVelocityWest	 =   5.54971; // +/- 0.000156657 cm/us All: West = 0.35491 +/- 0.250392
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54971 +/- 0.000156657
  return (TDataSet *)tableSet;// West = 5.54889 +/- 0.000272484 East = 5.55011 +/- 0.000191463
};
