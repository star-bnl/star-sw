TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 78015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56297; // +/- 1.07341e-05 cm/us All: East = 0.495843 +/- 0.0187144
  row.laserDriftVelocityWest	 =   5.56297; // +/- 1.07341e-05 cm/us All: West = 0.059239 +/- 0.00199699
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56297 +/- 1.07341e-05
  return (TDataSet *)tableSet;// West = 5.56302 +/- 1.08361e-05 East = 5.56055 +/- 7.84081e-05
};
