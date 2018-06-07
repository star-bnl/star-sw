TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 145009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55082; // +/- 1.77601e-05 cm/us All: East = -0.282718 +/- 0.0106612
  row.laserDriftVelocityWest	 =   5.55082; // +/- 1.77601e-05 cm/us All: West = 0.215572 +/- 0.00340063
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55082 +/- 1.77601e-05
  return (TDataSet *)tableSet;// West = 5.5505 +/- 1.88866e-05 East = 5.55324 +/- 5.22036e-05
};
