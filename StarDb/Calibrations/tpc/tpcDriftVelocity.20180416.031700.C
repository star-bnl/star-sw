TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55448; // +/- 9.24016e-06 cm/us All: East = -0.02641 +/- 0.00230595
  row.laserDriftVelocityWest	 =   5.55448; // +/- 9.24016e-06 cm/us All: West = 0.233313 +/- 0.00241329
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55448 +/- 9.24016e-06
  return (TDataSet *)tableSet;// West = 5.55376 +/- 1.33668e-05 East = 5.55515 +/- 1.27876e-05
};
