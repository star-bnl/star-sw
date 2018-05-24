TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53709; // +/- 4.77503e-05 cm/us All: East = -0.0654242 +/- 0.469197
  row.laserDriftVelocityWest	 =   5.53709; // +/- 4.77503e-05 cm/us All: West = 0.194947 +/- 0.0131604
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53709 +/- 4.77503e-05
  return (TDataSet *)tableSet;// West = 5.53682 +/- 5.0237e-05 East = 5.53959 +/- 0.000153677
};
