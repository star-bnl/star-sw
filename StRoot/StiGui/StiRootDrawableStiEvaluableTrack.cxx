//StiRootDrawableStiEvaluableTrack.cxx
//M.L. Miller (Yale Software)
//07/01

//STD
#include <algorithm>

//StEvent
#include "StEventTypes.h"

//Sti
#include "Sti/StiMapUtilities.h"

//StiGui
#include "StiTPolyLine3D.h"
#include "StiDisplayManager.h"
#include "StiRootDrawableStiEvaluableTrack.h"

using std::sort;

StiRootDrawableStiEvaluableTrack::StiRootDrawableStiEvaluableTrack()
{
    mremoved_each_event=true;
}

StiRootDrawableStiEvaluableTrack::~StiRootDrawableStiEvaluableTrack()
{
}

void StiRootDrawableStiEvaluableTrack::reset()
{
    StiEvaluableTrack::reset();
    const_hit_vector::clear();
}

void StiRootDrawableStiEvaluableTrack::fillHitsForDrawing()
{
    mline->SetPolyLine(0);
    mline->SetLineColor(mcolor);
    mline->ResetBit(kCanDelete);

    if (!msttrack) {
	cout <<"StiRootDrawableStiEvaluableTrack::fillHitsForDrawing() Error! msttrack==0"<<endl;
	return;
    }

    cout <<"Momentum:\t"<<msttrack->geometry()->momentum().mag()<<endl;

   //Draw primary vertex if this track belongs to one
   StPrimaryTrack* temp = dynamic_cast<StPrimaryTrack*>(msttrack);
   if (temp) { //She's a primary!
       const StThreeVectorF& pos = temp->vertex()->position();
       mline->SetNextPoint( pos.x(), pos.y(), pos.z() );
       //Find s at dca to vertex
       StPhysicalHelixD helix = msttrack->geometry()->helix();
       double sAtVertex = helix.pathLength( pos );
       //Now find last point
       double sAtEnd = helix.pathLength( msttrack->detectorInfo()->lastPoint() );
       //Now step from first hit to last hit
       if (sAtVertex>sAtEnd) {
	   cout <<"StiRootDrawableStiEvaluableTrack::fillHitsForDrawing()\tsAtVertex>sAtEnd.  ABORT"<<endl;
	   return;
       }
       for (double s=sAtVertex; s<=sAtEnd; s+=1.) {
	   mline->SetNextPoint( helix.x(s), helix.y(s), helix.z(s) );
       }
       StiDisplayManager::instance()->addDrawable(this);
       return;       
   }
   
   //Else draw as a global track
   StGlobalTrack* temp2 = dynamic_cast<StGlobalTrack*>(msttrack);
   if (temp2) { //She's a global!
       
       //Find s at dca to first point
       StPhysicalHelixD helix = msttrack->geometry()->helix();
       double sAtStart = helix.pathLength( msttrack->detectorInfo()->firstPoint() );
       //Now find last point
       double sAtEnd = helix.pathLength( msttrack->detectorInfo()->lastPoint() );
       //Now step from first hit to last hit
       if (sAtStart>sAtEnd) {
	   cout <<"StiRootDrawableStiEvaluableTrack::fillHitsForDrawing()\tsAtStart>sAtEnd.  ABORT"<<endl;
	   return;
       }
       for (double s=sAtStart; s<=sAtEnd; s+=1.) {
	   mline->SetNextPoint( helix.x(s), helix.y(s), helix.z(s) );
       }
       StiDisplayManager::instance()->addDrawable(this);
       return;              
   }
   
   return;
}
