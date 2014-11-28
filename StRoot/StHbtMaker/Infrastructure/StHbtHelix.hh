/***************************************************************************
 *
 * $Id: StHbtHelix.hh,v 1.1 2001/06/21 19:15:45 laue Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************
 *
 ***************************************************************************
 *
 * $Log: StHbtHelix.hh,v $
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
#ifndef StHbtHelix_hh
#define StHbtHelix_hh

#include "StHelix.hh"
typedef StHelix StHbtHelix;//!
#include "StPhysicalHelixD.hh"
typedef StPhysicalHelixD StHbtPhysicalHelix;//!
#endif
