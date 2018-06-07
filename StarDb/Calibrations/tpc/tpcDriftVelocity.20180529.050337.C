TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 149003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55046; // +/- 6.6894e-06 cm/us All: East = 0.197758 +/- 0.00190859
  row.laserDriftVelocityWest	 =   5.55046; // +/- 6.6894e-06 cm/us All: West = 0.163502 +/- 0.000993656
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55046 +/- 6.6894e-06
  return (TDataSet *)tableSet;// West = 5.55015 +/- 8.62086e-06 East = 5.55094 +/- 1.06048e-05
};
