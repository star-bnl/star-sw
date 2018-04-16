TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 101001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55373; // +/- 1.08566e-05 cm/us All: East = -5.65986 +/- 0.00202249
  row.laserDriftVelocityWest	 =   5.55373; // +/- 1.08566e-05 cm/us All: West = -5.12704 +/- 0.00729404
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55373 +/- 1.08566e-05
  return (TDataSet *)tableSet;// West = 5.55081 +/- 3.60368e-05 East = 5.55402 +/- 1.13856e-05
};
