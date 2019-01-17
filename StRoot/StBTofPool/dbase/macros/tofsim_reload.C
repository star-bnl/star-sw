// $Id: tofsim_reload.C,v 1.1 2019/01/16 16:42:29 geurts Exp $
// TOF annual database reload script
// expects table_reupload.C to be in the same directory
//
// $Log: tofsim_reload.C,v $
// Revision 1.1  2019/01/16 16:42:29  geurts
// Run 19 preps
//
// Revision 1.3  2013/01/22 05:57:40  geurts
// run 13 time stamp
//

void tofsim_reload(){
  // moved this line from the table_reupload script
  gROOT->Macro("LoadLogger.C");

  // DAQ Map and Tray Config (StBTofDaqMap, StBTofTables)
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofDaqMap\", \"ofl\", \"2018-12-09 23:59:59\",\"2018-12-10 00:00:00\"");
  gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTrayConfig\", \"ofl\", \"2018-12-09 23:59:59\",\"2018-12-10 00:00:00\"");

}
