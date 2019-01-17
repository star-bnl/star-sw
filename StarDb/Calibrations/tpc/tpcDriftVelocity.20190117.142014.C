TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 17021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55708; // +/- 9.46641e-06 cm/us All: East = -0.00634275 +/- 0.00331805
  row.laserDriftVelocityWest	 =   5.55708; // +/- 9.46641e-06 cm/us All: West = 0.256854 +/- 0.00196626
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55708 +/- 9.46641e-06
  return (TDataSet *)tableSet;// West = 5.5567 +/- 1.10445e-05 East = 5.55812 +/- 1.8377e-05
};
