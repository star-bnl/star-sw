TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 168010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54627; // +/- 5.44519e-06 cm/us All: East = -0.422877 +/- 0.00343828
  row.laserDriftVelocityWest	 =   5.54627; // +/- 5.44519e-06 cm/us All: West = -0.0695435 +/- 0.00100852
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54627 +/- 5.44519e-06
  return (TDataSet *)tableSet;// West = 5.54611 +/- 5.68386e-06 East = 5.54803 +/- 1.89902e-05
};
