// $Id: StChargedPionAnalysisMakerLinkDef.h,v 1.3.2.1 2016/05/23 18:33:18 jeromel Exp $

/*****************************************************************************
 * @author Adam Kocoloski
 *
 * cons won't generate these pragmas during its inspection of the source, but
 * we can force it to include them using this file.
 *****************************************************************************/

#ifdef __CINT__
#pragma link C++ class std::vector< StChargedPionJetParticle >;
#pragma link C++ class std::vector< StChargedPionMcTrack >;
#pragma link C++ class std::vector< StChargedPionJet >;
#pragma link C++ class std::vector< StMiniMcPair >;
#pragma link C++ class std::vector< StChargedPionPythiaRow >;
#endif

/*****************************************************************************
 * $Log: StChargedPionAnalysisMakerLinkDef.h,v $
 * Revision 1.3.2.1  2016/05/23 18:33:18  jeromel
 * Updates for SL12d / gcc44 embedding library - StDbLib, QtRoot update, new updated StJetMaker, StJetFinder, StSpinPool ... several cast fix to comply with c++0x and several cons related fixes (wrong parsing logic). Changes are similar to SL13b (not all ode were alike). Branch BSL12d_5_embed.
 *
 * Revision 1.4  2012/11/09 03:31:34  perev
 * Cleanup
 *
 * Revision 1.3  2008/12/29 15:58:28  kocolosk
 * removed commented code and added $Id: StChargedPionAnalysisMakerLinkDef.h,v 1.3.2.1 2016/05/23 18:33:18 jeromel Exp $/$Log: StChargedPionAnalysisMakerLinkDef.h,v $
 * removed commented code and added $Id: StChargedPionAnalysisMakerLinkDef.h,v 1.4 2012/11/09 03:31:34 perev Exp $/Revision 1.3.2.1  2016/05/23 18:33:18  jeromel
 * removed commented code and added $Id: StChargedPionAnalysisMakerLinkDef.h,v 1.4 2012/11/09 03:31:34 perev Exp $/Updates for SL12d / gcc44 embedding library - StDbLib, QtRoot update, new updated StJetMaker, StJetFinder, StSpinPool ... several cast fix to comply with c++0x and several cons related fixes (wrong parsing logic). Changes are similar to SL13b (not all ode were alike). Branch BSL12d_5_embed.
 * removed commented code and added $Id: StChargedPionAnalysisMakerLinkDef.h,v 1.4 2012/11/09 03:31:34 perev Exp $/
 * removed commented code and added $Id: StChargedPionAnalysisMakerLinkDef.h,v 1.3.2.1 2016/05/23 18:33:18 jeromel Exp $/Revision 1.4  2012/11/09 03:31:34  perev
 * removed commented code and added $Id: StChargedPionAnalysisMakerLinkDef.h,v 1.3.2.1 2016/05/23 18:33:18 jeromel Exp $/Cleanup
 * removed commented code and added $Id: StChargedPionAnalysisMakerLinkDef.h,v 1.3.2.1 2016/05/23 18:33:18 jeromel Exp $/ as needed
 *
 *****************************************************************************/

