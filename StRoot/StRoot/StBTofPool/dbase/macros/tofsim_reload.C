// 2019/12/10 Zaochen Ye modified it with the latest time stamp

// $Id: tofsim_reload.C,v 1.3 2021/01/28 17:16:35 zye20 Exp $
// TOF annual database reload script
// expects table_reupload.C to be in the same directory
//
// $Log: tofsim_reload.C,v $
// Revision 1.3  2021/01/28 17:16:35  zye20
// OBCVS: ----------------------------------------------------------------------
// :qi
//
// Revision 1.2  2019/12/15 02:40:57  zye20
// Initilizd BTOF 2020
//
// Revision 1.1  2019/01/16 16:42:29  geurts
// Run 19 preps
//
// Revision 1.3  2013/01/22 05:57:40  geurts
// run 13 time stamp
//

void tofsim_reload()
{
	// moved this line from the table_reupload script
	gROOT->Macro("LoadLogger.C");

	// DAQ Map and Tray Config (StBTofDaqMap, StBTofTables)
	gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofDaqMap\", \"ofl\", \"2020-12-10 23:59:59\",\"2020-12-13 00:00:00\"");
	gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofTrayConfig\", \"ofl\", \"2020-12-10 23:59:59\",\"2020-12-13 00:00:00\"");
	
	//-----------------------------------------------------------------------------------------------------
	//add by Zaochen, tables for sim to take resolutions from calibration to help provide a more realistic simulation of VPD and BTOF responses, they are input for St...SimMaker
	//-----------------------------------------------------------------------------------------------------
	gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"vpdSimParams\", \"ofl\", \"2020-12-10 23:59:59\",\"2020-12-13 00:00:00\"");
	gROOT->ProcessLine(".x table_reupload.C(\"Calibrations_tof\", \"tofSimResParams\", \"ofl\", \"2020-12-10 23:59:59\",\"2020-12-13 00:00:00\"");
	//-----------------------------------------------------------------------------------------------------
	//-----------------------------------------------------------------------------------------------------
}
