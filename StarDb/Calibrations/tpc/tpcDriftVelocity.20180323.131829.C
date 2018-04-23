TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 82021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53735; // +/- 1.3766e-05 cm/us All: East = -0.107989 +/- 6.29608
  row.laserDriftVelocityWest	 =   5.53735; // +/- 1.3766e-05 cm/us All: West = 0.241773 +/- 0.00246003
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53735 +/- 1.3766e-05
  return (TDataSet *)tableSet;// West = 5.53735 +/- 1.37662e-05 East = 5.53511 +/- 0.00262537
};
