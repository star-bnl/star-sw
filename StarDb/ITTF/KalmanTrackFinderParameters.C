TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // KalmanTrackFinderParameters 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_KalmanTrackFinderParameters")) return 0;
  KalmanTrackFinderParameters_st row;
  St_KalmanTrackFinderParameters *tableSet = 
    new St_KalmanTrackFinderParameters("KalmanTrackFinderParameters",2); // 0 - Inner, 1 - Outer
  //
  memset(&row,0,tableSet->GetRowSize());
  row.useMcAsRec                            = 1; // true
  row.useTrackFilter                        = 1;
  row.elossCalculated                       = 1;
  row.mcsCalculated                         = 1; 
  row.field                                 = 0.5; 
  row.maxNullCount                          = 13; 
  row.maxContiguousNullCount                =  8; 
  row.minContiguousHitCountForNullReset     =  2;
  //row.minSearchWindow                     = 1.6;
  //row.maxSearchWindow                     = 7.;
  //row.searchWindowScale                   = 10.;
  //row.maxChi2ForSelection                 = 10.;
  row.maxChi2Vertex                         = 1000.;
  row.massHypothesis                        = 0.139;
  //row.outerScaling                        = 1.;
  //row.innerScaling                        = 1.;

  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
