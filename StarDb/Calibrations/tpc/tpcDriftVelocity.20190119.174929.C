TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 19037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55658; // +/- 1.04233e-05 cm/us All: East = 0.183804 +/- 0.00495164
  row.laserDriftVelocityWest	 =   5.55658; // +/- 1.04233e-05 cm/us All: West = 0.581859 +/- 0.00200921
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55658 +/- 1.04233e-05
  return (TDataSet *)tableSet;// West = 5.5563 +/- 1.12202e-05 East = 5.55838 +/- 2.81604e-05
};
