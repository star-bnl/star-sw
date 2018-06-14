TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 158018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5514; // +/- 4.64727e-06 cm/us All: East = 0.229715 +/- 0.00193983
  row.laserDriftVelocityWest	 =   5.5514; // +/- 4.64727e-06 cm/us All: West = 0.169991 +/- 0.000910307
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5514 +/- 4.64727e-06
  return (TDataSet *)tableSet;// West = 5.55146 +/- 5.14326e-06 East = 5.55116 +/- 1.08467e-05
};
