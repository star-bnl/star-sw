TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52819; // +/- 1.05751e-05 cm/us All: East = -0.413697 +/- 0.0060203
  row.laserDriftVelocityWest	 =   5.52819; // +/- 1.05751e-05 cm/us All: West = 0.253474 +/- 0.00199167
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52819 +/- 1.05751e-05
  return (TDataSet *)tableSet;// West = 5.52788 +/- 1.10669e-05 East = 5.53154 +/- 3.58728e-05
};
