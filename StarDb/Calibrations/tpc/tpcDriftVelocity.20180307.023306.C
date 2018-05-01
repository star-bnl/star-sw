TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 65064
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53067; // +/- 5.21981e-05 cm/us All: East = 0.0943037 +/- 0.0420472
  row.laserDriftVelocityWest	 =   5.53067; // +/- 5.21981e-05 cm/us All: West = 0.445689 +/- 0.0433782
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53067 +/- 5.21981e-05
  return (TDataSet *)tableSet;// West = 5.52919 +/- 7.90768e-05 East = 5.53181 +/- 6.94878e-05
};
