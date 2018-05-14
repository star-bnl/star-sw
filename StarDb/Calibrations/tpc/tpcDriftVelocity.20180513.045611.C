TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 133003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54821; // +/- 8.68432e-06 cm/us All: East = -0.0801397 +/- 0.00265148
  row.laserDriftVelocityWest	 =   5.54821; // +/- 8.68432e-06 cm/us All: West = 0.236846 +/- 0.00191066
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54821 +/- 8.68432e-06
  return (TDataSet *)tableSet;// West = 5.54762 +/- 1.06577e-05 East = 5.54938 +/- 1.4981e-05
};
