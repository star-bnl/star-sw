TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100103
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52216; // +/- 1.33059e-05 cm/us All: East = -0.0932728 +/- 0.0619278
  row.laserDriftVelocityWest	 =   5.52216; // +/- 1.33059e-05 cm/us All: West = 0.00656386 +/- 0.00242302
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52216 +/- 1.33059e-05
  return (TDataSet *)tableSet;// West = 5.52216 +/- 1.33639e-05 East = 5.52259 +/- 0.000142918
};
