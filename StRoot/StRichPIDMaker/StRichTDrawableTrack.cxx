/**********************************************************
 * $Id: StRichTDrawableTrack.cxx,v 1.1 2000/06/16 02:37:12 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTDrawableTrack.cxx,v $
 *  Revision 1.1  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.2  2000/05/22 15:14:44  horsley
 *  modified StRichRings, StRichTDrawableTrack to comply with sun compiler
 *
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *
 **********************************************************/

#include "StRichTDrawableTrack.h"
#include "StRichTDrawableRings.h"
#include "StParticleDefinition.hh"
#include "StParticleTypes.hh"

#include "StRichRings.h"
#include "TPolyLine.h"
#include "StRichTrack.h"

#include "StThreeVectorD.hh"
#include "StThreeVector.hh"

#include <vector>


#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichTDrawableTrack::StRichTDrawableTrack() {}

StRichTDrawableTrack::StRichTDrawableTrack(StRichTrack* track) {

  mTrack = track;
  mProjectedMIP = 0;
  
  if (!mTrack) {
    cout << "StRichTDrawableTrack:: passed null pointer! " << endl;
    abort();
  }

   
  StPionMinus*  pionminus   = StPionMinus::instance();
  StKaonMinus*  kaonminus   = StKaonMinus::instance();
  StAntiProton* antiproton  = StAntiProton::instance();
  StPionPlus*  pionplus   = StPionPlus::instance();
  StKaonPlus*  kaonplus   = StKaonPlus::instance();
  StProton*    proton     = StProton::instance(); 
  
  mProjectedMIP = new StRichDrawableTMip(this);
  
  StRichRings pionminusRing(mTrack,pionminus);
  StRichRings pionplusRing(mTrack,pionplus);
  StRichRings kaonplusRing(mTrack,kaonplus);
  StRichRings kaonminusRing(mTrack,kaonminus);
  StRichRings antiprotonRing(mTrack,antiproton);
  StRichRings protonRing(mTrack,proton);
  
  mVectorRings.clear();
  mVectorRings.resize(0);
  
  if(mTrack->getCharge() < 0) // if negative
      {
	  mVectorRings.push_back(new StRichTDrawableRings(pionminusRing));
	  mVectorRings.push_back(new StRichTDrawableRings(kaonminusRing));
	  mVectorRings.push_back(new StRichTDrawableRings(antiprotonRing));
      }
  else
      {
	  mVectorRings.push_back(new StRichTDrawableRings(pionplusRing));
	  mVectorRings.push_back(new StRichTDrawableRings(kaonplusRing));
	  mVectorRings.push_back(new StRichTDrawableRings(protonRing));
      }
}

StRichTDrawableTrack::~StRichTDrawableTrack() {

    for(unsigned int i = 0;i < mVectorRings.size();i++)
	delete mVectorRings[i];

    mVectorRings.clear();
    mVectorRings.resize(0);
    
    delete mProjectedMIP;
}

StRichTDrawableRings* StRichTDrawableTrack::getRing(unsigned int i) {
    if(i < mVectorRings.size())
	return mVectorRings[i];
    else
	return 0;
}

StRichTDrawableRings* StRichTDrawableTrack::getRing(StParticleDefinition * def) {
    for(unsigned int i=0;i < mVectorRings.size();i++){
	if(mVectorRings[i]->getParticle() == def){
		return mVectorRings[i];
	}
    }
    return 0;
	    
}

StRichTrack* StRichTDrawableTrack::getTrack() {
  return mTrack;
}

Int_t StRichTDrawableTrack::numberOfRings() {
  return mVectorRings.size();
}

StRichDrawableTMip * StRichTDrawableTrack::getProjectedMIP(){
    return mProjectedMIP;}
