/**********************************************************
 * $Id: StRichDrawableTTrack.cxx,v 2.0 2000/08/09 16:28:03 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichDrawableTTrack.cxx,v $
 *  Revision 2.0  2000/08/09 16:28:03  gans
 *  Created New Maker for all drawable objects.
 *
 *  Revision 2.0  2000/08/09 16:28:03  gans
 *  Created New Maker for all drawable objects.
 *
 *  Revision 1.1  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.2  2000/05/22 15:14:44  horsley
 *  modified StRichRings, StRichDrawableTTrack to comply with sun compiler
 *
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *
 **********************************************************/

#include "StRichDrawableTTrack.h"

#include "StRichDrawableTRings.h"
#include "StParticleDefinition.hh"
#include "StParticleTypes.hh"

#include "StRichPIDMaker/StRichRings.h"
#include "TPolyLine.h"

#include "StRichPIDMaker/StRichTrack.h"

#include "StEventTypes.h"
#include "StPhysicalHelixD.hh"
#include "StPhysicalHelix.hh"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichDrawableTTrack::StRichDrawableTTrack() {}

StRichDrawableTTrack::StRichDrawableTTrack(StRichTrack* track) {

  mTrack = track;
  mProjectedMIP = 0;
  
  if (!mTrack) {
    cout << "StRichDrawableTTrack:: passed null pointer! " << endl;
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
  
  if(mTrack->getCharge() < 0) // if negative
      {
	  mVectorRings.push_back(new StRichDrawableTRings(pionminusRing));
	  mVectorRings.push_back(new StRichDrawableTRings(kaonminusRing));
	  mVectorRings.push_back(new StRichDrawableTRings(antiprotonRing));
      }
	  //   }

  /*
  else
      {
	  mVectorRings.push_back(new StRichDrawableTRings(pionplusRing));
      }
  */

}

StRichDrawableTTrack::~StRichDrawableTTrack() {

    for(unsigned int i = 0;i < mVectorRings.size();i++)
	delete mVectorRings[i];

    mVectorRings.clear();
    mVectorRings.resize(0);
    
    delete mProjectedMIP;
}

StRichDrawableTRings* StRichDrawableTTrack::getRing(unsigned int i) {
    if(i < mVectorRings.size())
	return mVectorRings[i];
    else
	return 0;
}

StRichDrawableTRings* StRichDrawableTTrack::getRing(StParticleDefinition * def) {
    for(unsigned int i=0;i < mVectorRings.size();i++){
	if(mVectorRings[i]->getParticle() == def){
		return mVectorRings[i];
	}
    }
    return 0;
	    
}

StRichTrack* StRichDrawableTTrack::getTrack() {
  return mTrack;
}

Int_t StRichDrawableTTrack::numberOfRings() {
  return mVectorRings.size();
}

StRichDrawableTMip * StRichDrawableTTrack::getProjectedMIP(){
    return mProjectedMIP;}
