TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 128043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55783; // +/- 6.13667e-06 cm/us All: East = 0.246033 +/- 0.00219255
  row.laserDriftVelocityWest	 =   5.55783; // +/- 6.13667e-06 cm/us All: West = 0.168935 +/- 0.00125003
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55783 +/- 6.13667e-06
  return (TDataSet *)tableSet;// West = 5.55794 +/- 7.05732e-06 East = 5.55748 +/- 1.24262e-05
};
