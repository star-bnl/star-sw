TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.555; // +/- 3.1053e-05 cm/us All: East = -5.85149 +/- 0.00549668
  row.laserDriftVelocityWest	 =   5.555; // +/- 3.1053e-05 cm/us All: West = -4.41515 +/- 0.152409
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.555 +/- 3.1053e-05
  return (TDataSet *)tableSet;// West = 5.54747 +/- 0.000229434 East = 5.55514 +/- 3.13414e-05
};
