TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98059
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55073; // +/- 6.37696e-05 cm/us All: East = -5.40761 +/- 0.329262
  row.laserDriftVelocityWest	 =   5.55073; // +/- 6.37696e-05 cm/us All: West = -4.99781 +/- 0.0375032
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55073 +/- 6.37696e-05
  return (TDataSet *)tableSet;// West = 5.54999 +/- 8.38777e-05 East = 5.55174 +/- 9.81662e-05
};
