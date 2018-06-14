TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 163043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54852; // +/- 5.8684e-06 cm/us All: East = -0.578554 +/- 0.00343266
  row.laserDriftVelocityWest	 =   5.54852; // +/- 5.8684e-06 cm/us All: West = 0.188728 +/- 0.001092
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54852 +/- 5.8684e-06
  return (TDataSet *)tableSet;// West = 5.54814 +/- 6.14881e-06 East = 5.5524 +/- 1.96568e-05
};
