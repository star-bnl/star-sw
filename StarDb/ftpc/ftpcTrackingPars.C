St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ftpcTrackingPars Allocated rows: 1  Used rows: 1  Row size: ??? bytes
//  Table: ftpcTrackingPars_st[0]--> ftpcTrackingPars_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ftpcTrackingPars")) return 0;
ftpcTrackingPars_st row;
St_ftpcTrackingPars *tableSet = new St_ftpcTrackingPars("ftpcTrackingPars",1);
//
memset(&row,0,tableSet->GetRowSize());

  // Vertex position
  row.maxVertexPosZWarning =  50.; // in cm
  row.maxVertexPosZError   = 100.; // in cm

  // Vertex reconstruction
  row.histoBins = 300;
  row.histoMin  = -75.0;  // in cm
  row.histoMax  = +75.0;  // in cm
  row.maxDcaVertex = 2.0;  // in cm
  row.minNumTracks = 1;      // must be >0 

  // Tracker
  row.rowSegments =  20;  // has to be fixed to the number of rows in both FTPCs
  row.phiSegments = 100;
  row.etaSegments = 200;
  
  // Tracking
  // the 4 indizes represent: 0: main vertex tracking                        
  //                          1: non vertex tracking
  //                          2: no field tracking
  //                          3: laser tracking

  row.laser[0] = 0;  // = kFALSE (type had to be changed from bool to long
  row.laser[1] = 0;  // = kFALSE (type had to be changed from bool to long
  row.laser[2] = 1;  // =  kTRUE (type had to be changed from bool to long
  row.laser[3] = 1;  // =  kTRUE (type had to be changed from bool to long

  row.vertexConstraint[0] = 1;  // =  kTRUE (type had to be changed from bool to long
  row.vertexConstraint[1] = 0;  // = kFALSE (type had to be changed from bool to long
  row.vertexConstraint[2] = 1;  // =  kTRUE (type had to be changed from bool to long
  row.vertexConstraint[3] = 0;  // = kFALSE (type had to be changed from bool to long

  row.maxTrackletLength[0] =  3;
  row.maxTrackletLength[1] =  3;
  row.maxTrackletLength[2] = 10;
  row.maxTrackletLength[3] = 10;

  row.minTrackLength[0] = 5;
  row.minTrackLength[1] = 5;
  row.minTrackLength[2] = 5;
  row.minTrackLength[3] = 5;

  row.rowScopeTracklet[0] = 2;  // was set to 1 for Markus' PhD thesis
  row.rowScopeTracklet[1] = 2;
  row.rowScopeTracklet[2] = 2;
  row.rowScopeTracklet[3] = 3;

  row.rowScopeTrack[0] = 3;  // was set to 2 for Markus' PhD thesis
  row.rowScopeTrack[1] = 3;
  row.rowScopeTrack[2] = 3;
  row.rowScopeTrack[3] = 3;

  row.phiScope[0] = 1;
  row.phiScope[1] = 1;
  row.phiScope[2] = 1;
  row.phiScope[3] = 2;

  row.etaScope[0] =  1;
  row.etaScope[1] =  3;
  row.etaScope[2] =  3;
  row.etaScope[3] = 15;

  row.maxDca[0] = 100.0;  // in cm
  row.maxDca[1] = 100.0;  // in cm
  row.maxDca[2] = 100.0;  // in cm
  row.maxDca[3] = 100.0;  // in cm

  // Tracklets
  row.maxAngleTracklet[0] = 0.015;  // in rad; was set to 0.007 for Markus' PhD thesis
  row.maxAngleTracklet[1] = 0.030;  // in rad
  row.maxAngleTracklet[2] = 0.030;  // in rad
  row.maxAngleTracklet[3] = 0.050;  // in rad

  // Tracks
  row.maxAngleTrack[0] = 0.030;  // in rad; was set to 0.007 for Markus' PhD thesis
  row.maxAngleTrack[1] = 0.080;
  row.maxAngleTrack[2] = 0.007;  // in rad; not used
  row.maxAngleTrack[3] = 0.007;  // in rad; not used

  row.maxCircleDist[0] = 0.05;  // in rad; was set to 0.03 for Markus' PhD thesis
  row.maxCircleDist[1] = 0.05;  // in rad
  row.maxCircleDist[2] = 0.03;  // in rad; not used
  row.maxCircleDist[3] = 0.03;  // in rad; not used

  row.maxLengthDist[0] = 30.;  // in cm
  row.maxLengthDist[1] = 70.;  // in cm
  row.maxLengthDist[2] = 30.;  // in cm; not used
  row.maxLengthDist[3] = 30.;  // in cm; not used
  
  // Split tracks
  row.maxDist       = 0.11;  // in cm
  row.minPointRatio = 0.50;
  row.maxPointRatio = 0.50;

tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
