TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // TrackingParameters 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TrackingParameters")) return 0;
  TrackingParameters_st row;
  St_TrackingParameters *tableSet = 
    new St_TrackingParameters("SvtTrackingParameters",1);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.MinSearch =  0.;
  row.MaxSearch =  0.;
  row.Scaling   =  0.;
  row.MaxChi2   =  0.;
  tableSet->AddAt(&row);
 // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
