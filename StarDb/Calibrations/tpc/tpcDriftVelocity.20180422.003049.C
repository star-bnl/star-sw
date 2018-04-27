TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 111046
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54389; // +/- 7.23957e-06 cm/us All: East = -0.0678798 +/- 0.00260927
  row.laserDriftVelocityWest	 =   5.54389; // +/- 7.23957e-06 cm/us All: West = 0.228502 +/- 0.00148555
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54389 +/- 7.23957e-06
  return (TDataSet *)tableSet;// West = 5.5435 +/- 8.33875e-06 East = 5.54507 +/- 1.45888e-05
};
