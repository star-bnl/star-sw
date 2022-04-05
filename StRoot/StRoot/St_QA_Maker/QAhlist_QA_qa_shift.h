// $Id: QAhlist_QA_qa_shift.h,v 2.9 2007/04/12 20:39:48 genevb Exp $
// $Log: QAhlist_QA_qa_shift.h,v $
// Revision 2.9  2007/04/12 20:39:48  genevb
// Cleanup (removal) of CalibVtx, Nfitpnt, Chisq1, Rich, histograms
//
// Revision 2.8  2007/03/13 18:37:53  genevb
// simplified names
//
// Revision 2.7  2002/02/12 18:41:59  genevb
// Additional FTPC histograms
//
// Revision 2.6  2002/01/26 03:04:07  genevb
// Fixed some problems with fcl histos
//
// Revision 2.5  2002/01/21 22:09:24  genevb
// Include some ftpc histograms from StFtpcClusterMaker
//
// Revision 2.4  2001/05/16 20:57:02  lansdell
// new histograms added for qa_shift printlist; some histogram ranges changed; StMcEvent now used in StEventQA
//
// Revision 2.3  2001/04/25 21:35:25  genevb
// Added V0 phi distributions
//
// Revision 2.2  2001/04/24 22:53:51  lansdell
// Removed redundant radial position of first hit histograms
//
// Revision 2.1  2000/08/25 16:04:10  genevb
// Introduction of files
//
//
///////////////////////////////////////////////////////////////////////
// Names of histograms to be plotted for dir=QA, analType=qa_shift
///////////////////////////////////////////////////////////////////////
// Note: Editing this file means that StAnalysisUtilities/StHistUtil
// must be recompiled

     "QaNullPrimVtx",
     "QaMultClass",
     "QaEvsumTotChg",
     "fcl_chargestepW",
     "fcl_chargestepE",
     "fcl_radialW",
     "fcl_radialE",

     "QaGtrkR0T",
     "QaGtrkPhi0T",
     "QaGtrkZ0T",
     "QaGtrkf0",
     "QaGtrkLengthT",
     "QaGtrkPsiT",
     "QaGtrkTanlT",
     "QaGtrkEtaT",
     "QaGtrkPtT",
     "QaGtrkChisq0T",
     "QaGtrkImpactT",
     "QaGtrkImpactrT",
     "QaGtrkPtVsEtaT",
     "QaGtrkTanlzf",
     "QaGtrkLengthVEtaT",
     "QaGtrkNPntLengthT",
     "QaGtrkFitPntLengthT",
     "QaGtrkChi0EtaT",
     "QaGtrkChi0PhiT",
     "QaPtrkTot",
     "QaPtrkGood",
     "QaPtrkNPntT",
     "QaPtrkNPntFitT",
     "QaPtrkPhi0T",
     "QaPtrkXfT",
     "QaPtrkYfT",
     "QaPtrkZfT",
     "QaPtrkf0",
     "QaPtrkRT",
     "QaPtrkPsiT",
     "QaPtrkTanlT",
     "QaPtrkPtT",
     "QaPtrkChisq0T",
     "QaPtrkImpactT",
     "QaPtrkImpactrT",
     "QaPtrkPtVsEtaT",
     "QaPtrkTanlzf",
     "QaPtrkLengthVEtaT",
     "QaPtrkNPntLengthT",
     "QaPtrkChi0EtaT",
     "QaPtrkPsiPhiT",
     "QaPidGlobtrkDstdedxPVsDedx",
     "QaVtxNum",
     "QaV0Vtx",
     "QaV0VtxPhiDist",
     "QaV0LambdaMass",
     "QaV0K0Mass",
     "QaXiVtxTot",
     "QaXiaMass",
     "QaKinkTot"
