TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 148012
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55082; // +/- 5.07932e-06 cm/us All: East = 0.115868 +/- 0.00269856
  row.laserDriftVelocityWest	 =   5.55082; // +/- 5.07932e-06 cm/us All: West = 0.139784 +/- 0.000957805
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55082 +/- 5.07932e-06
  return (TDataSet *)tableSet;// West = 5.55081 +/- 5.38708e-06 East = 5.55094 +/- 1.52461e-05
};
