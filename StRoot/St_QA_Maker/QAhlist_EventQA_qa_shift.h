// $Id: QAhlist_EventQA_qa_shift.h,v 2.53 2019/12/17 19:08:01 genevb Exp $
// $Log: QAhlist_EventQA_qa_shift.h,v $
// Revision 2.53  2019/12/17 19:08:01  genevb
// Add more ETOF histograms
//
// Revision 2.52  2019/03/26 15:29:37  genevb
// Introduce ETOF
//
// Revision 2.51  2019/03/14 02:31:52  genevb
// Introduce iTPC plots
//
// Revision 2.50  2018/05/02 21:07:40  genevb
// Initial accomodation for iTPC
//
// Revision 2.49  2018/03/21 02:46:29  genevb
// Updated list
//
// Revision 2.48  2015/04/02 19:53:46  genevb
// TPC dE/dx changes: Bethe-Bloch => Bichsel, and tighter cuts against pile-up tracks
//
// Revision 2.47  2014/07/22 20:39:28  genevb
// Add MTD to Offline QA
//
// Revision 2.46  2014/01/30 19:44:06  genevb
// Additional TPC histogram for monitoring gas contamination
//
// Revision 2.45  2013/03/20 20:17:37  genevb
// Run 13 edits
//
// Revision 2.44  2013/03/12 03:06:02  genevb
// Add FMS/FPD histograms for Run 13+
//
// Revision 2.43  2012/05/01 16:01:35  genevb
// correction on required detectors for EmcCat4 plots
//
// Revision 2.42  2012/04/23 18:17:30  genevb
// Modify list of BEMC hists for QA shift
//
// Revision 2.41  2012/03/02 23:56:48  genevb
// Place TPC sector plots in QA Shift
//
// Revision 2.40  2011/06/10 21:17:17  genevb
// Point Flag is EMC
//
// Revision 2.39  2011/06/07 18:49:53  genevb
// FTPC, Trigger removed from QA shift
//
// Revision 2.38  2011/03/15 21:02:14  genevb
// Remove some unused EmcCat4 plots
//
// Revision 2.37  2011/02/19 02:22:18  genevb
// Allow for specification of histogram usage by the required detector sets
//
// Revision 2.36  2011/02/07 20:25:56  genevb
// specify subsystems
//
// Revision 2.35  2010/01/04 19:44:52  genevb
// Remove V0s from QA shift set, revert to large multiplicity ranges
//
// Revision 2.34  2009/03/19 01:08:08  genevb
// Show both xy and rphi TPC hit hists
//
// Revision 2.33  2009/03/09 16:35:30  genevb
// Use small histogram ranges for pp qa_shift
//
// Revision 2.32  2008/02/05 22:51:50  genevb
// Correction on two histogram names for QA shift
//
// Revision 2.31  2007/05/02 20:46:40  genevb
// Additional SVT plots
//
// Revision 2.30  2007/04/12 20:39:48  genevb
// Cleanup (removal) of CalibVtx, Nfitpnt, Chisq1, Rich, histograms
//
// Revision 2.29  2007/04/07 04:40:29  genevb
// Remove fit pnts/tot; retitle log as log10
//
// Revision 2.28  2007/03/13 18:37:53  genevb
// simplified names
//
// Revision 2.27  2006/03/20 03:35:29  genevb
// FTPC: PtrkGood and PtrkPsi hists out, GtrkPadTime in
//
// Revision 2.26  2004/12/13 15:52:36  genevb
// Numerous updates: PMD, primtrk, FPD, QAShift lists
//
// Revision 2.25  2004/10/04 16:40:42  genevb
// FTPC radial histos
//
// Revision 2.24  2004/04/23 23:15:29  genevb
// Added signedDCA (Impact) plots for globals
//
// Revision 2.23  2004/03/17 00:35:14  genevb
// Left Mult off hist names in previous commit
//
// Revision 2.22  2004/03/15 23:32:14  genevb
// Add primary vertex check for event classes to QA Shift set
//
// Revision 2.21  2004/02/12 17:39:15  genevb
// Separate MinBias histos
//
// Revision 2.20  2004/02/12 05:03:03  genevb
// Year 4 AuAu changes. New SVT histos.
//
// Revision 2.19  2003/04/19 00:17:49  genevb
// Updated for dAu/pp running
//
// Revision 2.18  2003/01/02 19:39:47  genevb
// Comment out BBC/FPD histos; See diffs to previous version to restore
//
// Revision 2.17  2002/05/29 13:54:30  genevb
// Some changes to FTPC chisq histos
//
// Revision 2.16  2002/04/23 01:59:55  genevb
// Addition of BBC/FPD histos
//
// Revision 2.15  2002/02/12 18:41:59  genevb
// Additional FTPC histograms
//
// Revision 2.14  2002/01/26 03:04:07  genevb
// Fixed some problems with fcl histos
//
// Revision 2.13  2002/01/21 22:09:24  genevb
// Include some ftpc histograms from StFtpcClusterMaker
//
// Revision 2.12  2001/11/20 21:53:45  lansdell
// added x-y dist of hits, tpc east&west histos
//
// Revision 2.11  2001/08/29 20:45:15  genevb
// Trigger word histos
//
// Revision 2.10  2001/08/27 21:15:15  genevb
// fixed a typo
//
// Revision 2.9  2001/07/31 23:21:42  lansdell
// added last point, hit-helix histos
//
// Revision 2.8  2001/06/27 23:57:50  lansdell
// added geant-reco primvtx position histos to qa_shift list
//
// Revision 2.7  2001/05/29 23:23:05  lansdell
// removed impact param plots for FTPC from qa_shift list
//
// Revision 2.6  2001/05/25 16:31:20  lansdell
// more updates to qa shift histograms
//
// Revision 2.5  2001/05/24 01:48:13  lansdell
// qa_shift histograms updated
//
// Revision 2.4  2001/05/23 00:14:52  lansdell
// more changes for qa_shift histograms
//
// Revision 2.3  2001/05/16 20:57:02  lansdell
// new histograms added for qa_shift printlist; some histogram ranges changed; StMcEvent now used in StEventQA
//
// Revision 2.2  2001/04/24 22:53:50  lansdell
// Removed redundant radial position of first hit histograms
//
// Revision 2.1  2000/08/25 16:04:09  genevb
// Introduction of files
//
//
///////////////////////////////////////////////////////////////////////
// Names of histograms to be plotted for dir=EventQA, analType=qa_shift
///////////////////////////////////////////////////////////////////////
// Note: Editing this file means that StAnalysisUtilities/StHistUtil
// must be recompiled
// See StHistUtil::DetectorIn() for format of detector requirement

  "QaMultClass",

  ":itpc;tpx;tpc:QaPointTpc",
  ":svt:QaPointSvt",
  ":svt:QaPointXYSvt",
  ":svt:QaPointSvtLaser",
  ":itpc;tpx;tpc:QaPointRPTpcW",
  ":itpc;tpx;tpc:QaPointRPTpcE",
  ":tpx;tpc:QaTpcSector1",
  ":tpx;tpc:QaTpcSector2",
  ":tpx;tpc:QaTpcSector3",
  ":tpx;tpc:QaTpcSector4",
  ":tpx;tpc:QaTpcSector5",
  ":tpx;tpc:QaTpcSector6",
  ":tpx;tpc:QaTpcSector7",
  ":tpx;tpc:QaTpcSector8",
  ":tpx;tpc:QaTpcSector9",
  ":tpx;tpc:QaTpcSector10",
  ":tpx;tpc:QaTpcSector11",
  ":tpx;tpc:QaTpcSector12",
  ":tpx;tpc:QaTpcSector13",
  ":tpx;tpc:QaTpcSector14",
  ":tpx;tpc:QaTpcSector15",
  ":tpx;tpc:QaTpcSector16",
  ":tpx;tpc:QaTpcSector17",
  ":tpx;tpc:QaTpcSector18",
  ":tpx;tpc:QaTpcSector19",
  ":tpx;tpc:QaTpcSector20",
  ":tpx;tpc:QaTpcSector21",
  ":tpx;tpc:QaTpcSector22",
  ":tpx;tpc:QaTpcSector23",
  ":tpx;tpc:QaTpcSector24",
  ":itpc:QaiTpcSector1",
  ":itpc:QaiTpcSector2",
  ":itpc:QaiTpcSector3",
  ":itpc:QaiTpcSector4",
  ":itpc:QaiTpcSector5",
  ":itpc:QaiTpcSector6",
  ":itpc:QaiTpcSector7",
  ":itpc:QaiTpcSector8",
  ":itpc:QaiTpcSector9",
  ":itpc:QaiTpcSector10",
  ":itpc:QaiTpcSector11",
  ":itpc:QaiTpcSector12",
  ":itpc:QaiTpcSector13",
  ":itpc:QaiTpcSector14",
  ":itpc:QaiTpcSector15",
  ":itpc:QaiTpcSector16",
  ":itpc:QaiTpcSector17",
  ":itpc:QaiTpcSector18",
  ":itpc:QaiTpcSector19",
  ":itpc:QaiTpcSector20",
  ":itpc:QaiTpcSector21",
  ":itpc:QaiTpcSector22",
  ":itpc:QaiTpcSector23",
  ":itpc:QaiTpcSector24",
  ":tpx;tpc:QaPointZhits",
  ":tpx;tpc:QaPointPhiT",
  ":svt:QaPointZhitsS",
  ":svt:QaPointPhiS",
  ":tpx;tpc:QaPointPadrowT",
  ":svt:QaPointBarrelS",
  ":itpc;tpx;tpc:QaGtrkZfTS",
  ":itpc;tpx;tpc:QaGtrkPhifTS",
  ":itpc;tpx;tpc:QaGtrkZfT",
  ":itpc;tpx;tpc:QaGtrkPhifT",
  ":itpc;tpx;tpc:QaGtrkPadfTEW",
  ":itpc;tpx;tpc:QaGtrkRTS",
  ":itpc;tpx;tpc:QaGtrkRnmfTTS",
  ":itpc;tpx;tpc:QaGtrkPsiTTS",
  ":itpc;tpx;tpc:QaGtrkPtTTS",
  ":itpc;tpx;tpc:QaGtrkEtaTTS",
  ":itpc;tpx;tpc:QaGtrkChisq0T",
  ":itpc;tpx;tpc:QaGtrkFlag",
  ":itpc;tpx;tpc:QaGtrkGood",
  ":itpc;tpx;tpc:QaGtrkNPntFitTTS",
  ":itpc;tpx;tpc:QaGtrkGoodTTS",
  ":itpc;tpx;tpc:QaGtrkFitPntLTTS",
  ":itpc;tpx;tpc:QaGtrkSImpactT",
  ":itpc;tpx;tpc:QaGtrkImpactTTS",
  ":itpc;tpx;tpc:QaGtrkImpactrTTS",
  ":itpc;tpx;tpc:QaGtrkDetId",
  ":itpc;tpx;tpc:QaGtrkTanlzf",
  ":itpc;tpx;tpc:QaGtrkTanlzfTS",
  ":itpc;tpx;tpc:QaPtrkPsiTTS",
  ":itpc;tpx;tpc:QaPtrkPtTTS",
  ":itpc;tpx;tpc:QaPtrkEtaTTS",
  ":itpc;tpx;tpc:QaPtrkMeanPtTTS",
  ":itpc;tpx;tpc:QaPtrkMeanEtaTTS",
  ":itpc;tpx;tpc:QaPtrkGood",
  ":itpc;tpx;tpc:QaPtrkGoodTTS",
  ":itpc;tpx;tpc:QaPtrkChisq0TTS",
  ":itpc;tpx;tpc:QaPtrkFlag",
  ":itpc;tpx;tpc:QaPtrkGlob",
  ":itpc;tpx;tpc:QaPtrkFitPntLTTS",
  ":svt:QaPtrkSvtLoc",
  ":itpc;tpx;tpc:QaNullPrimVtxMult",
  ":itpc;tpx;tpc:QaVtxPrXY",
  ":itpc;tpx;tpc:QaVtxPrZ",
  ":itpc;tpx;tpc:QaGtrkDcaBeamZ1",
  ":itpc;tpx;tpc:QaGtrkDcaBeamZ2",
  ":itpc;tpx;tpc:QaGtrkZdcaZf",
  ":itpc;tpx;tpc:QaGtrkZdcaPsi",
  ":itpc;tpx;tpc:QaGtrkZdcaTanl",
  ":itpc;tpx;tpc:QaPidGlobtrkDstdedxPVsDedx",
  ":itpc;tpx;tpc:QaDedxBBTTS",
  ":itpc;tpx;tpc:QaDedxBTTS",
  //":itpc;tpx;tpc:Z3A",
  ":itpc;tpx;tpc:QaEvsumTotChg",
  ":itpc;tpx;tpc:QaGtrkRZf0",
  ":tpx,svt;tpc,svt:QaGtrkRZf0TS",
  ":tpx,svt:QaPtrkRZf0",
  ":tpx,svt;tpc,svt:QaPtrkRZf0TS",
  ":itpc:TPC_adc_sec_inner",
  ":itpc:TPC_adc_sec_outer",
  ":emc:bemcClNum",
  ":emc:bemcClEnergy",
  ":emc:bemcEta",
  ":emc:bemcPhi",
  ":bsmd:bsmdeClNum",
  ":bsmd:bsmdeEta",
  ":bsmd:bsmdpClNum",
  ":bsmd:bsmdpPhi",
  ":emc,bsmd:EmcCat4_Point_Energy",
  ":emc,bsmd:EmcCat4_Point_Eta",
  ":emc,bsmd:EmcCat4_Point_Phi",
  ":emc,bsmd:EmcCat4_Sigma_Eta",
  ":emc,bsmd:EmcCat4_Sigma_Phi",
  ":emc,bsmd: Point Flag",
  "QaGRpvtxDx",
  "QaGRpvtxDy",
  "QaGRpvtxDz",
  "QaBbcAdcES",
  "QaBbcAdcEL",
  "QaBbcAdcWS",
  "QaBbcAdcWL",
  "QaBbcTdcES",
  "QaBbcTdcEL",
  "QaBbcTdcWS",
  "QaBbcTdcWL",
  ":fms:fms_qt_channel_adc_crate_1",
  ":fms:fms_qt_channel_adc_crate_2",
  ":fms:fms_qt_channel_adc_crate_3",
  ":fms:fms_qt_channel_adc_crate_4",
  ":fpd:fpd_channel_adc",
  ":fpd:QaFpdTop0",
  ":fpd:QaFpdTop1",
  ":fpd:QaFpdBottom0",
  ":fpd:QaFpdBottom1",
  ":fpd:QaFpdSouth0",
  ":fpd:QaFpdSouth1",
  ":fpd:QaFpdNorth0",
  ":fpd:QaFpdNorth1",
  ":fpd:QaFpdSums0",
  ":fpd:QaFpdSums1",
  ":fpd:QaFpdSums2",
  ":fpd:QaFpdSums3",
  ":fpd:QaFpdSums4",
  ":fpd:QaFpdSums5",
  ":fpd:QaFpdSums6",
  //":fpd:QaFpdSums7",
  ":mtd:QaMtdNHits",
  ":mtd:QaMtdHitMap",
  ":mtd:QaMtdNMatchHits",
  ":mtd:QaMtdMatchHitMap",
  ":tof:QaTofPID",
  ":etof:etofHit_tof",
  ":etof,tof:averageTimeDiff_etofHits_btofHits",
  ":etof,tof:multiplicity_etofHits_btofHits",
  ":etof:multiplicity_etofHits_epdEast",
  ":etof,tpx:A_eTofHits_globalXY",
  ":etof,tpx:B_intersectionMult_etofMult",
  ":etof,tpx:G_matchCand_beta_signmom",
  ":etof,tpx:G_matchCand_timeOfFlight_pathLength",
  ":etof,tpx:G_primary_Intersection_validMatch",
  ":etof,tpx:H_matchCand_t0corr_1d"
