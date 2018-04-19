TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55588; // +/- 1.144e-05 cm/us All: East = -0.140562 +/- 0.00260303
  row.laserDriftVelocityWest	 =   5.55588; // +/- 1.144e-05 cm/us All: West = 0.177734 +/- 0.00326648
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55588 +/- 1.144e-05
  return (TDataSet *)tableSet;// West = 5.55479 +/- 1.85586e-05 East = 5.55654 +/- 1.45287e-05
};
