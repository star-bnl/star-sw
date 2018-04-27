TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53477; // +/- 2.03626e-05 cm/us All: East = -0.390742 +/- 0.00462107
  row.laserDriftVelocityWest	 =   5.53477; // +/- 2.03626e-05 cm/us All: West = 0.446375 +/- 0.00695149
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53477 +/- 2.03626e-05
  return (TDataSet *)tableSet;// West = 5.53166 +/- 3.62545e-05 East = 5.53621 +/- 2.46112e-05
};
