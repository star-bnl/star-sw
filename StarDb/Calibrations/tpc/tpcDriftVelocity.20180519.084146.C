TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55426; // +/- 4.77628e-06 cm/us All: East = 0.318777 +/- 0.00441537
  row.laserDriftVelocityWest	 =   5.55426; // +/- 4.77628e-06 cm/us All: West = 0.164062 +/- 0.000858722
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55426 +/- 4.77628e-06
  return (TDataSet *)tableSet;// West = 5.5543 +/- 4.86076e-06 East = 5.55322 +/- 2.57313e-05
};
