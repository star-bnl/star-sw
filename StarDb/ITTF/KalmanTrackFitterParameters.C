TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // KalmanTrackFitterParameters 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_KalmanTrackFitterParameters")) return 0;
  KalmanTrackFitterParameters_st row;
  St_KalmanTrackFitterParameters *tableSet = 
    new St_KalmanTrackFitterParameters("KalmanTrackFitterParameters",1);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.maxChi2  = 3.;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
