TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5377; // +/- 5.82948e-06 cm/us All: East = 0.0205708 +/- 0.00312867
  row.laserDriftVelocityWest	 =   5.5377; // +/- 5.82948e-06 cm/us All: West = 0.124405 +/- 0.00106615
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5377 +/- 5.82948e-06
  return (TDataSet *)tableSet;// West = 5.53781 +/- 5.98489e-06 East = 5.53571 +/- 2.57476e-05
};
