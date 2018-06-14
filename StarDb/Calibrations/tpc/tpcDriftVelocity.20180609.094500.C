TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 160011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54975; // +/- 5.96717e-06 cm/us All: East = -0.0490227 +/- 0.00364726
  row.laserDriftVelocityWest	 =   5.54975; // +/- 5.96717e-06 cm/us All: West = 0.135337 +/- 0.00111195
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54975 +/- 5.96717e-06
  return (TDataSet *)tableSet;// West = 5.54965 +/- 6.26219e-06 East = 5.55069 +/- 1.96728e-05
};
