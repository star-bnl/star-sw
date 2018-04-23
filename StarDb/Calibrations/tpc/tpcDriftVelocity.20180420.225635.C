TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 110041
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55399; // +/- 5.27178e-06 cm/us All: East = -0.0890112 +/- 0.00293565
  row.laserDriftVelocityWest	 =   5.55399; // +/- 5.27178e-06 cm/us All: West = 0.145093 +/- 0.000986168
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55399 +/- 5.27178e-06
  return (TDataSet *)tableSet;// West = 5.55385 +/- 5.57561e-06 East = 5.55515 +/- 1.6191e-05
};
