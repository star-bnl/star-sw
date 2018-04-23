TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53396; // +/- 9.42857e-06 cm/us All: East = -0.00885891 +/- 21.0742
  row.laserDriftVelocityWest	 =   5.53396; // +/- 9.42857e-06 cm/us All: West = 0.191138 +/- 0.00167902
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53396 +/- 9.42857e-06
  return (TDataSet *)tableSet;// West = 5.53396 +/- 9.42857e-06 East = 5.55773 +/- 0.0294968
};
