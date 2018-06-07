TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 148027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55088; // +/- 3.90588e-06 cm/us All: East = 0.256995 +/- 0.00146055
  row.laserDriftVelocityWest	 =   5.55088; // +/- 3.90588e-06 cm/us All: West = 0.107686 +/- 0.000789437
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55088 +/- 3.90588e-06
  return (TDataSet *)tableSet;// West = 5.55107 +/- 4.4459e-06 East = 5.55027 +/- 8.17687e-06
};
