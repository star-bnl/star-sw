TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137044
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53767; // +/- 5.85055e-05 cm/us All: East = 0.370104 +/- 0.236234
  row.laserDriftVelocityWest	 =   5.53767; // +/- 5.85055e-05 cm/us All: West = 0.0764457 +/- 0.0106331
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53767 +/- 5.85055e-05
  return (TDataSet *)tableSet;// West = 5.53665 +/- 7.02062e-05 East = 5.54001 +/- 0.000105842
};
