TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 133054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54742; // +/- 1.09391e-05 cm/us All: East = -0.294363 +/- 0.0040776
  row.laserDriftVelocityWest	 =   5.54742; // +/- 1.09391e-05 cm/us All: West = 0.235097 +/- 0.00223305
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54742 +/- 1.09391e-05
  return (TDataSet *)tableSet;// West = 5.54678 +/- 1.24607e-05 East = 5.54957 +/- 2.28439e-05
};
