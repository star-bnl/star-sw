/***************************************************************************
 *
 * $Id: StHbtEnumeration.hh,v 1.3 2003/01/08 19:43:12 perev Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************
 *
 ***************************************************************************
 *
 * $Log: StHbtEnumeration.hh,v $
 * Revision 1.3  2003/01/08 19:43:12  perev
 * CleanUp
 *
 * Revision 1.2  2001/09/05 20:41:42  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 * Revision 1.1  2001/06/21 19:15:45  laue
 * Modified fiels:
 *   CTH.hh : new constructor added
 *   StHbtEvent, StHbtKink, StHbtTrack : constructors from the persistent
 *                                   (TTree) classes added
 *   StHbtLikeSignAnalysis : minor changes, for debugging
 *   StHbtTypes: split into different files
 * Added files: for the new TTree muDst's
 *   StExceptions.cxx StExceptions.hh StHbtEnumeration.hh
 *   StHbtHelix.hh StHbtHisto.hh StHbtString.hh StHbtTFile.hh
 *   StHbtTTreeEvent.cxx StHbtTTreeEvent.h StHbtTTreeKink.cxx
 *   StHbtTTreeKink.h StHbtTTreeTrack.cxx StHbtTTreeTrack.h
 *   StHbtTTreeV0.cxx StHbtTTreeV0.h StHbtVector.hh
 *
 *
 ***************************************************************************/
#ifndef StHbtEnumeration_hh
#define StHbtEnumeration_hh

enum StHbtParticleType {hbtUndefined, hbtTrack, hbtV0, hbtKink, hbtXi};
enum StHbtIOMode {hbtRead, hbtWrite};

#endif
