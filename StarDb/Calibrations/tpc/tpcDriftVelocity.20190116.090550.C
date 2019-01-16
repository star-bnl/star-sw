TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 16009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5582; // +/- 1.44124e-05 cm/us All: East = -1.7548 +/- 0.00381688
  row.laserDriftVelocityWest	 =   5.5582; // +/- 1.44124e-05 cm/us All: West = -1.06182 +/- 0.00358906
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5582 +/- 1.44124e-05
  return (TDataSet *)tableSet;// West = 5.55638 +/- 1.98285e-05 East = 5.56024 +/- 2.09852e-05
};
