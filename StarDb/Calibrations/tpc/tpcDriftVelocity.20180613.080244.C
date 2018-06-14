TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 164009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54872; // +/- 6.00651e-06 cm/us All: East = -0.420308 +/- 0.00402752
  row.laserDriftVelocityWest	 =   5.54872; // +/- 6.00651e-06 cm/us All: West = 0.161844 +/- 0.00109445
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54872 +/- 6.00651e-06
  return (TDataSet *)tableSet;// West = 5.54854 +/- 6.17762e-06 East = 5.55169 +/- 2.56984e-05
};
