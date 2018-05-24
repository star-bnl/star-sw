TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55586; // +/- 8.24072e-06 cm/us All: East = -0.47151 +/- 0.00282666
  row.laserDriftVelocityWest	 =   5.55586; // +/- 8.24072e-06 cm/us All: West = -0.185696 +/- 0.00101673
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55586 +/- 8.24072e-06
  return (TDataSet *)tableSet;// West = 5.55511 +/- 9.59557e-06 East = 5.55798 +/- 1.60856e-05
};
