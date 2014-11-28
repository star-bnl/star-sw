/***************************************************************************
 *
 * $Id: StHbtCheckPdgIdList.h,v 1.2 2001/06/23 21:55:17 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 * $Log: StHbtCheckPdgIdList.h,v $
 * Revision 1.2  2001/06/23 21:55:17  laue
 * StHbtCheckPdgIdList can take can not check for mother,particle,daughter
 * Some output turned off
 *
 * Revision 1.1  2000/05/25 21:23:03  laue
 * Adding to CVS. Tool to select particle Ids from event generator input.
 *
 **************************************************************************/


#ifndef StHbtCheckPdgIdList_hh
#define StHbtCheckPdgIdList_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include <list>

#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<int, allocator<int> >            pdgIdList;       
typedef list<int, allocator<int> >::iterator  pdgIdListIterator;
#else
typedef list<int>            pdgIdList;
typedef list<int>::iterator  pdgIdListIterator;
#endif

class StHbtCheckPdgIdList {

public:

  pdgIdList* mAcceptedParticles; //!
  pdgIdList* mAcceptedMothers;   //!
  pdgIdList* mAcceptedDaughters; //!

  StHbtCheckPdgIdList();
  virtual ~StHbtCheckPdgIdList();

  StHbtString Report();

  void AddAcceptedParticle( int pdgCode );
  void AddAcceptedMother( int pdgCode );
  void AddAcceptedDaughter( int pdgCode );
  int  CheckPdgIdLists();                              // returns 1 if one of the three lists is not empty
  int  CheckPdgIdList( pdgIdList* list);               // returns 1 if list is not empty
  int  CheckPdgIdList( pdgIdList* list, int pdgCode ); // returns 1 if list is not empty or requested pdgCode is in list
  int  CheckPdgIdList( int  pdgCode, int motherPdgCode, int daughterPdgCode);

#ifdef __ROOT__
  ClassDef(StHbtCheckPdgIdList, 0)
#endif
};

#endif

