TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 126042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56704; // +/- 7.7418e-06 cm/us All: East = -0.181028 +/- 0.00220857
  row.laserDriftVelocityWest	 =   5.56704; // +/- 7.7418e-06 cm/us All: West = 0.214871 +/- 0.00178164
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56704 +/- 7.7418e-06
  return (TDataSet *)tableSet;// West = 5.56618 +/- 1.00231e-05 East = 5.56831 +/- 1.21892e-05
};
