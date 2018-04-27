TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 87042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51808; // +/- 8.38965e-06 cm/us All: East = -0.542294 +/- 0.00820634
  row.laserDriftVelocityWest	 =   5.51808; // +/- 8.38965e-06 cm/us All: West = 0.370777 +/- 0.00152309
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51808 +/- 8.38965e-06
  return (TDataSet *)tableSet;// West = 5.51793 +/- 8.52216e-06 East = 5.52287 +/- 4.77611e-05
};
