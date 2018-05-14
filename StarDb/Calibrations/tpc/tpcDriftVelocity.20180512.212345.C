TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 132065
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54809; // +/- 7.31566e-06 cm/us All: East = -0.0538087 +/- 0.00218824
  row.laserDriftVelocityWest	 =   5.54809; // +/- 7.31566e-06 cm/us All: West = 0.218949 +/- 0.00161436
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54809 +/- 7.31566e-06
  return (TDataSet *)tableSet;// West = 5.5476 +/- 9.05257e-06 East = 5.54903 +/- 1.24204e-05
};
