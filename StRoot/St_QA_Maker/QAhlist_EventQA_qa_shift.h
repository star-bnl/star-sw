// $Id: QAhlist_EventQA_qa_shift.h,v 2.32 2008/02/05 22:51:50 genevb Exp $
// $Log: QAhlist_EventQA_qa_shift.h,v $
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

  "QaMultClass",
  "QaTrigWord",
  "QaTrigBits",
  "fcl_chargestepW",
  "fcl_chargestepE",
  "fcl_radialW",
  "fcl_radialE",

  "QaPointTpc",
  "QaPointSvt",
  "QaPointXYSvt",
  "QaPointSvtLaser",
  "QaPointFtpc",
  "QaPointXYTpcE",
  "QaPointXYTpcW",
  "QaPointXYFtpcE",
  "QaPointXYFtpcW",
  "QaPointPadTimeFtpcW",
  "QaPointPadTimeFtpcE",
  "QaPointZhits",
  "QaPointPhiT",
  "QaPointZhitsS",
  "QaPointPhiS",
  "QaPointPadrowT",
  "QaPointBarrelS",
  "QaPointPlaneF",
  "QaGtrkZfTS",
  "QaGtrkPhifTS",
  "QaGtrkZfT",
  "QaGtrkPhifT",
  "QaGtrkXfYfFE",
  "QaGtrkXfYfFW",
  "QaGtrkPadfTEW",
  "QaGtrkRTS",
  "QaGtrkRnmfTTS",
  "QaGtrkPsiTTS",
  "QaGtrkPtTTS",
  "QaGtrkEtaTTS",
  "QaGtrkPsiF",
  "QaGtrkPtF",
  "QaGtrkEtaF",
  "QaGtrkPF",
  "QaGtrkChisq0T",
  "QaGtrkFlag",
  "QaGtrkGood",
  "QaGtrkNPntFitTTS",
  "QaGtrkNPntF",
  "QaGtrkGoodTTS",
  "QaGtrkGoodF",
  "QaGtrkFitPntLTTS",
  "QaGtrkSImpactT",
  "QaGtrkImpactTTS",
  "QaGtrkImpactrTTS",
  "QaGtrkImpactrF",
  "QaGtrkDetId",
  "QaGtrkTanlzf",
  "QaGtrkTanlzfTS",
  "QaPtrkPsiTTS",
  "QaPtrkPtTTS",
  "QaPtrkEtaTTS",
  "QaPtrkPtF",
  "QaPtrkEtaF",
  "QaPtrkMeanPtTTS",
  "QaPtrkMeanEtaTTS",
  "QaPtrkMeanPtF",
  "QaPtrkMeanEtaF",
  "QaPtrkGood",
  "QaPtrkGoodTTS",
  "QaPtrkChisq0TTS",
  "QaPtrkFlag",
  "QaPtrkGlob",
  "QaPtrkFitPntLTTS",
  "QaPtrkSvtLoc",
  "QaNullPrimVtxMult",
  "QaVtxPrXY",
  "QaVtxPrZ",
  "QaVtxFtpcETpcXY",
  "QaVtxFtpcETpcZ",
  "QaVtxFtpcWTpcXY",
  "QaVtxFtpcWTpcZ",
  "QaGtrkDcaBeamZ1",
  "QaGtrkDcaBeamZ2",
  "QaGtrkZdcaZf",
  "QaGtrkZdcaPsi",
  "QaGtrkZdcaTanl",
  "QaPidGlobtrkDstdedxPVsDedx",
  "QaDedxBBTTS",
  "QaEvsumTotChg",
  "QaEvsumTotChgF",
  "QaV0Vtx",
  "QaV0VtxRDist",
  "QaV0VtxZDist",
  "QaV0VtxPhiDist",
  "QaV0LambdaMass",
  "QaV0K0Mass",
  "QaXiVtxTot",
  "QaKinkTot",
  "QaGtrkRZf0",
  "QaGtrkRZf0TS",
  "QaPtrkRZf0",
  "QaPtrkRZf0TS",
  "bemcClNum",
  "bemcClEnergy",
  "bemcEta",
  "bemcPhi",
  "bsmdeClNum",
  "bsmdeEta",
  "bsmdpClNum",
  "bsmdpPhi",
  "EmcCat4_Point_Energy",
  "EmcCat4_Point_Eta",
  "EmcCat4_Point_Phi",
  "EmcCat4_Sigma_Eta",
  "EmcCat4_Sigma_Phi",
  "EmcCat4_Delta_Eta",
  "EmcCat4_Delta_Phi",
  "EmcCat4_Points_Multiplicity",
  "EmcCat4_Track_Momenta",
  " Point Flag",
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
  "QaFpdTop0",
  "QaFpdTop1",
  "QaFpdBottom0",
  "QaFpdBottom1",
  "QaFpdSouth0",
  "QaFpdSouth1",
  "QaFpdNorth0",
  "QaFpdNorth1",
  "QaFpdSums0",
  "QaFpdSums1",
  "QaFpdSums2",
  "QaFpdSums3",
  "QaFpdSums4",
  "QaFpdSums5",
  "QaFpdSums6",
  "QaFpdSums7"
