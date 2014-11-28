// $Id: tof_reload.C,v 1.4 2014/02/19 16:21:18 geurts Exp $
// TOF annual database reload script
// expects table_reupload.C to be in the same directory
//
// $Log: tof_reload.C,v $
// Revision 1.4  2014/02/19 16:21:18  geurts
// *** empty log message ***
//
// Revision 1.3  2013/01/22 05:57:40  geurts
// run 13 time stamp
//

void tof_reload(){

  // moved this line from the table_reupload script
  gROOT->Macro("LoadLogger.C");

  // INL (StBTofINLCorr)
  //  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTDIGOnTray\", \"ofl\", \"2012-12-19 00:00:00\",\"2012-12-20 00:00:00\"");
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofINLSCorr\", \"ofl\", \"2013-12-19 00:00:00\",\"2013-12-20 00:00:00\"");

  // alignment (StBTofGeometry)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofGeomAlign\", \"ofl\", \"2013-12-19 00:00:00\",\"2013-12-20 00:00:00\"");

  // DAQ Map and Tray Config (StBTofDaqMap, StBTofTables)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofDaqMap\", \"ofl\", \"2013-12-19 00:00:00\",\"2013-12-20 00:00:00\"");
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTrayConfig\", \"ofl\", \"2013-12-19 00:00:00\",\"2013-12-20 00:00:00\"");

  // trigger window, vpd delay (StBTofSortRawHit)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTrgWindow\", \"ofl\", \"2013-12-19 00:00:00\",\"2013-12-20 00:00:00\"");
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"vpdDelay\", \"ofl\", \"2013-12-19 00:00:00\",\"2013-12-20 00:00:00\"");

  // tofStatus (StBTofTables)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofStatus\", \"ofl\", \"2013-12-19 00:00:00\",\"2013-12-20 00:00:00\"");

  // calibration: T0, Slewing, Zhit (StBTofCalibMaker)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTotbCorr\", \"ofl\", \"2013-12-19 00:00:00\",\"2013-12-20 00:00:00\"");
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofZbCorr\", \"ofl\", \"2013-12-19 00:00:00\",\"2013-12-20 00:00:00\"");
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTOffset\", \"ofl\", \"2013-12-19 00:00:00\",\"2013-12-20 00:00:00\"");

  // calibration: start (StVpdCalibMaker)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"vpdTotCorr\", \"ofl\", \"2013-12-19 00:00:00\",\"2013-12-20 00:00:00\"");
}

