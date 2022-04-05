#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StGammaTrack.h"

ClassImp(StGammaTrack);

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StGammaTrack::StGammaTrack()
{
    id = 0;
    flag = -1;
    charge = 0;
    nhits = 0;
    dEdx = 0;
}

//////////////////////////////////////////////////
//             Overloaded Constructor           //
//////////////////////////////////////////////////
StGammaTrack::StGammaTrack(StMuTrack* track)
{

    id = track->id();
    flag = track->flag();
    charge = track->charge();
    nhits = track->nHitsFit();
    dEdx = track->dEdx() / keV; 
    type = track->type();
    momentum = track->momentum().xyz();
    dca = track->dcaGlobal().xyz();
    helix = track->helix();
    outerHelix = track->outerHelix();
    
}

//////////////////////////////////////////////////
//                  Destructor                  //
//////////////////////////////////////////////////
StGammaTrack::~StGammaTrack()
{}


//////////////////////////////////////////////////
//   Project the track out to radius R.  See    //
//  StRoot/StEmcUtil/projection/StEmcPosition.h //
//////////////////////////////////////////////////
TVector3 StGammaTrack::positionAtRadius(Double_t radius) const
{
    
    static const pair<double, double> VALUE(999999999., 999999999.); // No solution
    
    if(outerHelix.origin().perp() < radius) 
    {
    
        pair<double, double> ss = outerHelix.pathLength(radius);
        
        if(finite(ss.first) && finite(ss.second) && ss != VALUE) 
        {
        
            double s = 0;
            
            if( (ss.first > 0 && ss.second > 0) || (ss.first >= 0 && ss.second < 0) )
            {
                s = ss.first;
            }
            else if(ss.first < 0 && ss.second >= 0)
            {
                s = ss.second;
            }
            
            if(s > 0) return outerHelix.at(s).xyz();
            
        }
    }

    return TVector3(0,0,0);

}
        

//////////////////////////////////////////////////
//      Project the track out to distance z     //
//////////////////////////////////////////////////
TVector3 StGammaTrack::positionAtZ(Double_t z) const
{

    if( fabs(outerHelix.origin().z()) < fabs(z) ) 
    {
    
        StThreeVector<double> r(0, 0, z);
        StThreeVector<double> n(0, 0, 1);
        
        double s = outerHelix.pathLength(r, n);
        
        if (finite(s) && s != StHelix::NoSolution && s > 0)
        { 
            return outerHelix.at(s).xyz();
        }
        
    }
    
    return TVector3(0, 0, 0);
  
}
