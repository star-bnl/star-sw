/***************************************************************************
 *
 * $Id: StHbtCheckPdgIdList.cxx,v 1.3 2001/06/23 21:55:17 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 * $Log: StHbtCheckPdgIdList.cxx,v $
 * Revision 1.3  2001/06/23 21:55:17  laue
 * StHbtCheckPdgIdList can take can not check for mother,particle,daughter
 * Some output turned off
 *
 * Revision 1.2  2000/07/16 21:38:22  laue
 * StHbtCoulomb.cxx StHbtSectoredAnalysis.cxx : updated for standalone version
 * StHbtV0.cc StHbtV0.hh : some cast to prevent compiling warnings
 * StHbtParticle.cc StHbtParticle.hh : pointers mTrack,mV0 initialized to 0
 * StHbtIOBinary.cc : some printouts in #ifdef STHBTDEBUG
 * StHbtEvent.cc : B-Field set to 0.25Tesla, we have to think about a better
 *                 solution
 *
 * Revision 1.1  2000/05/25 21:23:03  laue
 * Adding to CVS. Tool to select particle Ids from event generator input.
 *
 **************************************************************************/
#include "StHbtMaker/Infrastructure/StHbtCheckPdgIdList.h"

//#if !(ST_NO_NAMESPACES)
//  using namespace units;
//#endif

#ifdef __ROOT__
  ClassImp(StHbtCheckPdgIdList)
#endif


inline double min(double a, double b) { return (a<b) ? a : b; }
inline double max(double a, double b) { return (a<b) ? b : a; }

//__________________
StHbtCheckPdgIdList::StHbtCheckPdgIdList(){
  mAcceptedParticles = new pdgIdList;
  mAcceptedMothers = new pdgIdList;
  mAcceptedDaughters = new pdgIdList;
}
//__________________
StHbtCheckPdgIdList::~StHbtCheckPdgIdList(){
  delete mAcceptedParticles;
  delete mAcceptedMothers;
  delete mAcceptedDaughters;
}
//__________________
StHbtString StHbtCheckPdgIdList::Report(){
  StHbtString temp = "\n This is the StHbtCheckPdgIdList\n";  temp += "---> EventCuts in Reader: ";
  char Ctemp[10];
  temp += "\n  Accepted Particles (pdgId): ";
  pdgIdListIterator iter;
  for (iter=mAcceptedParticles->begin(); iter!=mAcceptedParticles->end(); iter++) {
    sprintf(Ctemp," %i",*iter);
    temp += Ctemp;
  }
  temp += "\n  Accepted Mothers (pdgId): ";
  for (iter=mAcceptedMothers->begin(); iter!=mAcceptedMothers->end(); iter++) {
    sprintf(Ctemp," %i",*iter);
    temp += Ctemp;
  }
  temp += "\n  Accepted Daughters (pdgId): ";
  for (iter=mAcceptedDaughters->begin(); iter!=mAcceptedDaughters->end(); iter++) {
    sprintf(Ctemp," %i",*iter);
    temp += Ctemp;
  }
  temp += "\n";
  return temp;
}
//__________________
void StHbtCheckPdgIdList::AddAcceptedParticle( int pdgCode ){
  mAcceptedParticles->push_back( pdgCode );
    }
//__________________
void StHbtCheckPdgIdList::AddAcceptedMother( int pdgCode ){
  mAcceptedMothers->push_back( pdgCode );
}
//__________________
void StHbtCheckPdgIdList::AddAcceptedDaughter( int pdgCode ){
  mAcceptedDaughters->push_back( pdgCode );
}
//__________________
int StHbtCheckPdgIdList::CheckPdgIdLists(){
  return mAcceptedParticles->size()+mAcceptedMothers->size()+mAcceptedDaughters->size();
}
//__________________
int StHbtCheckPdgIdList::CheckPdgIdList( pdgIdList* list){
  if (list->size()==0) return 1; // if there are no specified particles at all, accept everything
  return 0; // there is a list of particles, to you have to check them
}
//__________________
int StHbtCheckPdgIdList::CheckPdgIdList( pdgIdList* list, int pdgCode ){
  if (list->size()==0) return 1; // if there are no specified particles at all, accept everything
  for (pdgIdListIterator iter=list->begin(); iter!=list->end(); iter++)
    if ( (*iter)==pdgCode) return 1; // particle accepted
  return 0; // pdgCode refused
}
//__________________
int StHbtCheckPdgIdList::CheckPdgIdList( int pdgCode, int motherPdgCode, int daughterPdgCode ){
  return CheckPdgIdList(mAcceptedParticles,pdgCode) 
    * CheckPdgIdList(mAcceptedMothers,motherPdgCode) 
    * CheckPdgIdList(mAcceptedDaughters,daughterPdgCode); 
}
