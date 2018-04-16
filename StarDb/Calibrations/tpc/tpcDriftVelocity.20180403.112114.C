TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52963; // +/- 9.52012e-06 cm/us All: East = -0.795293 +/- 0.00723807
  row.laserDriftVelocityWest	 =   5.52963; // +/- 9.52012e-06 cm/us All: West = -0.070255 +/- 0.00175877
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52963 +/- 9.52012e-06
  return (TDataSet *)tableSet;// West = 5.52942 +/- 9.79128e-06 East = 5.53344 +/- 4.07347e-05
};
