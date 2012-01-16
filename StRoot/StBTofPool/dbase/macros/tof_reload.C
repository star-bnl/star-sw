// TOF annual database reload script
// expects table_reupload.C to be in the same directory

void tof_reload(){

  // moved this line from the table_reupload script
  gROOT->Macro("LoadLogger.C");

  // INL (StBTofINLCorr)
  //  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTDIGOnTray\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofINLSCorr\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");

  // alignment (StBTofGeometry)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofGeomAlign\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");

  // DAQ Map and Tray Config (StBTofDaqMap, StBTofTables)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofDaqMap\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTrayConfig\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");

  // trigger window, vpd delay (StBTofSortRawHit)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTrgWindow\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"vpdDelay\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");

  // tofStatus (StBTofTables)
  // gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofStatus\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");

  // calibration: T0, Slewing, Zhit (StBTofCalibMaker)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTotbCorr\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofZbCorr\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTOffset\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");

  // calibration: start (StVpdCalibMaker)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"vpdTotCorr\", \"ofl\", \"2011-12-19 00:00:00\",\"2011-12-20 00:00:00\"");
}

