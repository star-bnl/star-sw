TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55089; // +/- 1.30713e-05 cm/us All: East = 0.0349835 +/- 0.00293266
  row.laserDriftVelocityWest	 =   5.55089; // +/- 1.30713e-05 cm/us All: West = 0.413539 +/- 0.00383746
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55089 +/- 1.30713e-05
  return (TDataSet *)tableSet;// West = 5.54965 +/- 2.08984e-05 East = 5.55169 +/- 1.67527e-05
};
