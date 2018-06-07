TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 147030
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55001; // +/- 4.97415e-06 cm/us All: East = 0.133281 +/- 0.00170741
  row.laserDriftVelocityWest	 =   5.55001; // +/- 4.97415e-06 cm/us All: West = 0.132852 +/- 0.00100149
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55001 +/- 4.97415e-06
  return (TDataSet *)tableSet;// West = 5.55003 +/- 5.81332e-06 East = 5.54995 +/- 9.61079e-06
};
