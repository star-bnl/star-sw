TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54965; // +/- 2.25451e-05 cm/us All: East = -0.139859 +/- 0.0156169
  row.laserDriftVelocityWest	 =   5.54965; // +/- 2.25451e-05 cm/us All: West = 0.277846 +/- 0.00457097
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54965 +/- 2.25451e-05
  return (TDataSet *)tableSet;// West = 5.54925 +/- 2.47732e-05 East = 5.55155 +/- 5.43943e-05
};
