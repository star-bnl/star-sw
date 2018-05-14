TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 130084
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55007; // +/- 6.57799e-06 cm/us All: East = 0.0937005 +/- 0.00234282
  row.laserDriftVelocityWest	 =   5.55007; // +/- 6.57799e-06 cm/us All: West = 0.131103 +/- 0.000861866
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55007 +/- 6.57799e-06
  return (TDataSet *)tableSet;// West = 5.54979 +/- 7.62019e-06 East = 5.55089 +/- 1.30307e-05
};
