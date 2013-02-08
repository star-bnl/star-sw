/**********************************************************************
 *
 * $Id: StEStructTrack.cxx,v 1.12 2013/02/08 19:32:52 prindle Exp $
 *
 * Author: Jeff Porter merge of code from Aya Ishihara and Jeff Reid
 *
 **********************************************************************
 *
 * Description:  Persistent track information
 *
 **********************************************************************/

#include "StEStructTrack.h"
#include "StPhysicalHelix.hh"
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

ClassImp(StEStructTrack)

// Setting BField is important when we read EStruct format events and we need to reconstruct the helix.
// We should somehow be getting BField from the file, or somewhere. Can change event to event in principle.
// For now we force person running EStruct correlation analysis to set BField appropriately.
// Set it to 0 when reading MuDst (and helix is calculated properly) or to the correct signed valuem to recalculate the helix.
// For 200 GeV AuAu run in 2011 following is good.
//Float_t StEStructTrack::BField = 4.9845;
Float_t StEStructTrack::BField = 4.9845;


StThreeVectorD StEStructTrack::PrimVertex = StThreeVectorD(0,0,0);
StEStructTrack::StEStructTrack(StEStructTrack *track) : TObject() {
  mPx = track->Px();
  mPy = track->Py();
  mPz = track->Pz();

  mEta = track->Eta();
  mPhi = track->Phi();

  mBxPrimary = track->BxPrimary();
  mByPrimary = track->ByPrimary();
  mBzPrimary = track->BzPrimary();

  mBxGlobal = track->BxGlobal();
  mByGlobal = track->ByGlobal();
  mBzGlobal = track->BzGlobal();

  mPIDe_dEdx  = track->PIDe_dEdx();
  mPIDpi_dEdx = track->PIDpi_dEdx();
  mPIDp_dEdx  = track->PIDp_dEdx();
  mPIDk_dEdx  = track->PIDk_dEdx();
  mPIDd_dEdx  = track->PIDd_dEdx();

  mPIDe_ToF  = track->PIDe_ToF();
  mPIDpi_ToF = track->PIDpi_ToF();
  mPIDp_ToF  = track->PIDp_ToF();
  mPIDk_ToF  = track->PIDk_ToF();
  mPIDd_ToF  = track->PIDd_ToF();

  mChi2 = track->Chi2();
  mBeta = track->beta();
  mDedx = track->Dedx();
  mAssignedMass=track->AssignedMass();

  mNFitPoints = track->NFitPoints();
  mNFoundPoints = track->NFoundPoints();
  mNMaxPoints = track->NMaxPoints();

  mDetectorID = track->DetectorID();
  mFlag = track->Flag();

  mCharge = track->Charge();

  mMap[0] = track->TopologyMapData(0);
  mMap[1] = track->TopologyMapData(1);
  mTPCNHits = track->TopologyMapTPCNHits();

  mHelix = track->Helix();
  //
  // check to see if one can complete ... requires event level information
  // such as bfield. If so, complete and set, if not set incomplete.
  //
 if(track->isComplete()){
    FillTransientData();
    mStartPos=track->StartPos();
    FillTpcReferencePoints();
    mIsComplete=true;
 } else {
   mIsComplete=false;
 }
}

//----------------------------------------------------------
void StEStructTrack::FillTransientData(){

  if (0 != StEStructTrack::BField) {
      StThreeVectorD momentum(mPx,mPy,mPz);
      StThreeVectorD origin(mBxPrimary,mByPrimary,mBzPrimary);
      origin += StEStructTrack::PrimVertex;
      mHelix = StPhysicalHelixD(momentum,origin,StEStructTrack::BField*kilogauss,mCharge);
  }

  evalPt();
  evalPtot();
  evalYt();
  evalXt();
  evalCurvature();
  evalFourMomentum();
  evalMass();
  evalPID();
  
};

//----------------------------------------------------------
void StEStructTrack::evalYt(){
  float _r=mPt/0.13957;
  mYt = log(sqrt(1+_r*_r)+_r);

  mytbin=(int) floor((mYt-1.0)/0.5);
  if(mytbin>6)mytbin=6;  // Why 6?
  if(mytbin<0)mytbin=0;
};

//----------------------------------------------------------
void StEStructTrack::evalXt(){
  //
  // cut and paste from Aya's code
  //
  // Aya calls this Xmt, it doesn't seem to be what she used for the XtXt paper, so I'll use the standard instead
  /*float PionMass = 0.139;
  float Temperature = 0.25;
  float Minimum = (1+(PionMass/Temperature))*exp(-PionMass/Temperature);
  float mtOnly = sqrt((mPx*mPx)+(mPy*mPy)+PionMass*PionMass);
  mXt=1-(1+(mtOnly/Temperature))*exp(-mtOnly/Temperature)/Minimum;*/

  float PionMass = 0.139;
  float mt = Mt(PionMass);
  mXt = 1 - exp( -1*(mt-PionMass)/0.4 );

};


//----------------------------------------------------------
void StEStructTrack::evalPID(){
    // Combine dEdx and ToF information
    // Our numbering: (should we maybe be more forward thinking, with room for e, Lambda, K0??)
    //     pi = 1
    //     K  = 2
    //     p  = 3

    // If there is ToF information and dEdx information require it to be consistent.
    // For Hijing we seem to have mDedx = mBeta = 0. Use tight cuts on mPIDpi_dEdx etc.
    float pi = fabs(mPIDpi_dEdx);
    float k  = fabs(mPIDk_dEdx);
    float p  = fabs(mPIDp_dEdx);
    int dpi     = 0;
    int dK      = 0;
    int dp      = 0;
    int mPID_dEdx = 0;
    if (mDedx > 0) {
        mPID_dEdx = 1;
        if (fabs(pi)<2) {
            dpi = 1;
            if (fabs(k)>2 && fabs(p)>2) {
                dpi = 2;
                mPID_dEdx = 2;
            }
        }
        if (fabs(k)<2) {
            dK = 1;
            if (fabs(pi)>2  && fabs(p)>2) {
                dK = 2;
                mPID_dEdx = 2;
            }
        }
        if (fabs(p)<2) {
            dp = 1;
            if (fabs(pi)>2  && fabs(k)>2) {
                dp = 2;
                mPID_dEdx = 2;
            }
        }
    }

    // Something like 30% of tracks have no ToF?
    // Is a ToF hit that is not within a PID band more important than a missing ToF hit?
// The widths of the mass bands expand rapidly with p_t, so pi and K have a big overlap by 1.5 GeV/c.
// Try a hard mass cut as it appears the yields fall off so there is little ambiguity.
    double tnpi = mPIDpi_ToF;
    double tnK  = mPIDk_ToF;
    double tnp  = mPIDp_ToF;
    int tpi    = 0;
    int tK     = 0;
    int tp     = 0;
    int mPID_ToF = 0;
    if (mBeta > 0) {
        mPID_ToF = 1;
        if (0.05 < mMass && mMass <= 0.25) {
            mPID_ToF = 2;
            tpi = 2;
        } else if (0.4 < mMass && mMass <= 0.65) {
            mPID_ToF = 2;
            tK = 2;
        } else if (0.8 < mMass && mMass <= 1.15) {
            mPID_ToF = 2;
            tp = 2;
        }
    }

    if (mPID_dEdx==2 && mPID_ToF==2) {
        // Both ToF and dEdx identified particle. Require they agree. 
        if (dpi==2 && tpi==2) {
            mPID = 1;
        } else if (dK==2 && tK==2) {
            mPID = 2;
        } else if (dp==2 && tp==2) {
            mPID = 3;
        } else {
            mPID = 0;
        }
    } else if (mPID_dEdx==2) {
        // Only dEdx identified particle. ToF can be ambiguous but must be consistent if it is there
        if (mPID_ToF==1) {
            if (dpi==2 && tpi==1) {
                mPID = 1;
            } else if (dK==2 && tK==1) {
                mPID = 2;
            } else if (dp==2 && tp==1) {
                mPID = 3;
            } else {
                mPID = 0;
            }
        } else {
            if (dpi==2) {
                mPID = 1;
            } else if (dK==2) {
                mPID = 2;
            } else if (dp==2) {
                mPID = 3;
            } else {
                mPID = 0;
            }
        }
    } else if (mPID_ToF==2) {
        // Only ToF identified particle. dEdx can be ambiguous but must be consistent if it is there
        if (mPID_dEdx==1) {
            if (tpi==2 && dpi==1) {
                mPID = 1;
            } else if (tK==2 && dK==1) {
                mPID = 2;
            } else if (tp==2 && dp==1) {
                mPID = 3;
            } else {
                mPID = 0;
            }
        } else {
            if (tpi==2) {
                mPID = 1;
            } else if (tK==2) {
                mPID = 2;
            } else if (tp==2) {
                mPID = 3;
            } else {
                mPID = 0;
            }
        }
    } else if (0 == mDedx && 0 == mBeta) {
        if (pi < 0.1 && tnpi < 0.1) {
            mPID = 1;
        } else if (k < 0.1 && tnK < 0.1) {
            mPID = 2;
        } else if (p < 0.1 && tnp < 0.1) {
            mPID = 3;
        } else {
            mPID = 0;
        }
    } else {
        mPID = 0;
    }
};

//----------------------------------------------------------
void StEStructTrack::evalCurvature(){
    // store helix curvature.
    // Seems that curvature from helix is _not_ signed.
    // Sign of curvature is -helicity. (When magnetic field along +Z direction
    // helicity of a positive particle is negative.)
    double b = mHelix.h();
    double c = mHelix.curvature();
    mCurvature = -mHelix.h()*fabs(mHelix.curvature());
};


//----------------------------------------------------------
void StEStructTrack::evalFourMomentum(const float mass){

  float lMass=mass;
  if(lMass<=0)lMass=0.13957;

  mFourMomentum.setPx(mPx);
  mFourMomentum.setPy(mPy);
  mFourMomentum.setPz(mPz);
  mFourMomentum.setE(sqrt(mPt*mPt+mPz*mPz+lMass*lMass));

}

//----------------------------------------------------------
void StEStructTrack::FillTpcReferencePoints() {
  // Uses fitted helix to calculate intersection points in the TPC

  static StThreeVectorF WestEnd(0.,0.,200.);
  static StThreeVectorF EastEnd(0.,0.,-200.);
  static StThreeVectorF EndCapNormal(0.,0.,1.0);

  // In this use, pathLength(r) returns the helix path length to the intersection of a cylinder with radius r.
  // There are 2 mathematical solutions, so both are returned in the pairD class  where first < second.
  // If the first is <0 it is unphysical, so we would use the second.

  // The exit point is a special case, we need to find if the track exited the side or endcaps of TPC
  pairD candidates = mHelix.pathLength(200.0);
  double sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
  double endLength = mHelix.pathLength(WestEnd,EndCapNormal);
  int endcap;
  if (endLength < 0.0) {
      endcap = +1;
      endLength = mHelix.pathLength(EastEnd,EndCapNormal);
  } else {
      endcap = -1;
  }
  double firstExitLength = endLength;
  if (endLength > sideLength) {
      mEndCapOuter = 0;
      firstExitLength = sideLength;
  } else {
      mEndCapOuter = endcap;
  }
  mNominalTpcExitPoint = mHelix.at(firstExitLength);

  candidates = mHelix.pathLength(50.0);
  sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
  mNominalTpcEntrancePoint = mHelix.at(sideLength);

  // With cuts |\eta| < 1 and |V_z| < 50cm all tracks should cross 127cm radius before
  // intersecting endcap. (Need to double check this is true for lowest momentum helix we accept.)
  mMidTPCRadius = 127.0;
  candidates = mHelix.pathLength(mMidTPCRadius);
  sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
  mMidTpcPoint = mHelix.at(sideLength);

  // Add OuterMid point at 163.5. This is to help with my crossing cut for LS tracks that have different pt.
  // Possible track may have excited via endcap at this radius.
  mOuterMidTPCRadius = 163.5;
  candidates = mHelix.pathLength(mOuterMidTPCRadius);
  sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
  firstExitLength = endLength;
  if (endLength > sideLength) {
      mEndCapOuterMid = 0;
      firstExitLength = sideLength;
  } else {
      mEndCapOuterMid = endcap;
  }
  mOuterMidTpcPoint = mHelix.at(firstExitLength);

  // Store maximum radius this track will get to. Not all tracks get to specific radii.
  double curve = mHelix.curvature();
  if (curve > 0.000001) {
      mMaxRadius = 2/curve;
  } else {
      mMaxRadius = 99999.0;   // Arbitrary number bigger than anything we care about.
  }
  // Store radius of track intersection with endcap.
  StThreeVectorF vendcap = mHelix.at(endLength);
  mEndCapRadius = sqrt(vendcap.x()*vendcap.x() + vendcap.y()*vendcap.y());

  mIsComplete=true;  // finished with calculations

}


//----------------------------------------------------------
//  older stuff ... should look at some time in the future ...
//----------------------------------------------------------

Float_t StEStructTrack::Pt() const { return mPt; };
Float_t StEStructTrack::Ptot() const { return mPtot; };

Float_t StEStructTrack::Mt(Float_t mass) const { 
  return sqrt((mPt*mPt)+(mass*mass)); 
}

Float_t StEStructTrack::E(Float_t mass) const { 
  return sqrt((mPt*mPt)+(mPz*mPz)+(mass*mass)); 
}

Float_t StEStructTrack::Yt(Float_t mass) const { 
  if (0 == mass) {
      return mYt;
  } else {
      Float_t E = this->E(mass);
      return 0.5*log((E+mPt)/(E-mPt));
  }
}

Float_t StEStructTrack::Eta(Float_t mass) const {
  if (0 == mass) {
      return mEta;
  } else {
      return this->Rapidity(mass);
  }
}

Float_t StEStructTrack::Rapidity(Float_t mass) const { 
  Float_t E = this->E(mass);
  return 0.5*log((E+mPz)/(E-mPz)); 
}

Float_t StEStructTrack::Dca() const { 
  return (sqrt((mBxPrimary*mBxPrimary)+(mByPrimary*mByPrimary)+(mBzPrimary*mBzPrimary))); 
}

Float_t StEStructTrack::DcaPrimary() const { 
  return (sqrt((mBxPrimary*mBxPrimary)+(mByPrimary*mByPrimary)+(mBzPrimary*mBzPrimary))); 
}

Float_t StEStructTrack::DcaGlobal() const { 
  return (sqrt((mBxGlobal*mBxGlobal)+(mByGlobal*mByGlobal)+(mBzGlobal*mBzGlobal))); 
}

/**********************************************************************
 *
 * $Log: StEStructTrack.cxx,v $
 * Revision 1.12  2013/02/08 19:32:52  prindle
 * Added "Triggered" histograms in StEStruct2ptCorrelations.
 * Protected against using tracks cuts in StEStruct2ptCorrelations when reading EStruct format events.
 * Added comment in EventMaker/StEStructTrack.cxx pointing out need to set BField correctly
 * when reading EStruct format events. (This should be read from file somehow, but...)
 *
 * Revision 1.11  2012/11/16 21:24:37  prindle
 * Changes to support reading/writing of EStructEvent. Fill helix as transient and
 * get BField from file (?).
 *
 * Revision 1.10  2011/08/02 20:36:57  prindle
 *   Event: modifications for ZDCCoincidence
 *   Track: big changes in evalPID. These should be superseded when TOF-dEdx
 *          space is understood better.
 *
 * Revision 1.9  2010/09/02 21:26:29  prindle
 *   Track: Added ToF pid information, modify dEdx, add combined pid code.
 *
 * Revision 1.8  2010/03/02 21:47:18  prindle
 *   Support to retrieve track radius when it crosses endplate
 *   Add way to retrieve centrality
 *
 * Revision 1.7  2008/12/02 23:45:48  prindle
 * Added curvature and calculation of OuterMidTpcPoint.
 *
 * Revision 1.6  2006/02/22 22:06:07  prindle
 * Removed all references to multRef (?)
 *
 * Revision 1.5  2005/09/14 17:21:19  msd
 * Simplified helix fitting by taking helix from mudst instead of calculating from scratch
 *
 * Revision 1.4  2005/07/07 19:31:13  fisyak
 * Add default for mHelix
 *
 * Revision 1.3  2005/03/03 01:32:03  porter
 * fixed a bug setting 4-momentum and added data (+accessors)
 * to the track class
 *
 * Revision 1.2  2004/06/28 23:24:11  chunhuih
 *
 * added 'const' specification to some member functions, including some of the
 * return types, so that they can be used by a const StEStructTrack object.
 *
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/











