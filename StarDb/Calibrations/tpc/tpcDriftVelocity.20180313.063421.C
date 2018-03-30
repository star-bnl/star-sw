TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 72010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54245; // +/- 1.46797e-05 cm/us All: East = -0.115673 +/- 0.00814342
  row.laserDriftVelocityWest	 =   5.54245; // +/- 1.46797e-05 cm/us All: West = -0.0816361 +/- 0.00279913
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54245 +/- 1.46797e-05
  return (TDataSet *)tableSet;// West = 5.54242 +/- 1.55283e-05 East = 5.54265 +/- 4.50246e-05
};
