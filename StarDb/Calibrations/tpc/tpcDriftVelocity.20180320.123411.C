TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55575; // +/- 7.65101e-06 cm/us All: East = 1.9865 +/- 0.0064798
  row.laserDriftVelocityWest	 =   5.55575; // +/- 7.65101e-06 cm/us All: West = 1.99258 +/- 0.0014035
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55575 +/- 7.65101e-06
  return (TDataSet *)tableSet;// West = 5.55574 +/- 7.84516e-06 East = 5.55594 +/- 3.46052e-05
};
