TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53519; // +/- 1.41946e-05 cm/us All: East = -0.197889 +/- 0.0108939
  row.laserDriftVelocityWest	 =   5.53519; // +/- 1.41946e-05 cm/us All: West = 0.294789 +/- 0.00270609
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53519 +/- 1.41946e-05
  return (TDataSet *)tableSet;// West = 5.53501 +/- 1.46741e-05 East = 5.53775 +/- 5.5986e-05
};
