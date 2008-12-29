// $Id: StChargedPionAnalysisMakerLinkDef.h,v 1.3 2008/12/29 15:58:28 kocolosk Exp $

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
 * Revision 1.3  2008/12/29 15:58:28  kocolosk
 * removed commented code and added Id and Log as needed
 *
 *****************************************************************************/

