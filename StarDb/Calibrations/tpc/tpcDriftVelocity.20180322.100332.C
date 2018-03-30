TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54852; // +/- 9.69209e-06 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.54852; // +/- 9.69209e-06 cm/us All: West = -1.17569 +/- 0.00174039
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54852 +/- 9.69209e-06
  return (TDataSet *)tableSet;// West = 5.54852 +/- 9.69209e-06 East = -999 +/- 999
};
