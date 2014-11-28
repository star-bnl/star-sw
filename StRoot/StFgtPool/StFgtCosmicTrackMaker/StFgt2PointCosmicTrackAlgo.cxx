/***************************************************************************
 *
 * $Id: StFgt2PointCosmicTrackAlgo.cxx,v 1.1 2012/01/31 23:25:35 avossen Exp $ 
 * Author: C. K. Riley (ckriley@bnl.gov), Oct. 11 2011 
 *
 ***************************************************************************
 *
 * Description:  2PointAlgo CosmicTrack making algorithm takes 2 points 
 * from outer quads, fits actual hit to expected hit on middle quad
 *
 ***************************************************************************
 *
 * $Log: StFgt2PointCosmicTrackAlgo.cxx,v $
 * Revision 1.1  2012/01/31 23:25:35  avossen
 * moved StFgtCosmicTrackMaker to StFgtPool
 *
 * Revision 1.9  2011/11/25 20:24:59  ckriley
 * now will look at all possible point combinations for tracks and pick the best one
 *
 * Revision 1.8  2011/11/16 22:15:07  ckriley
 * now looks at all points on quadrants and gets best track
 *
 * Revision 1.7  2011/11/01 18:50:13  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.6  2011/10/25 15:45:18  ckriley
 * minor change
 *
 * Revision 1.4  2011/10/20 17:13:44  ckriley
 * major update -> headers, tracks stored in StFgtEvent instead of StFgtDisc, changes to trackmaker and algorithms
 *
 *
 **************************************************************************/

#include "StFgt2PointCosmicTrackAlgo.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtPoint.h"


StFgt2PointCosmicTrackAlgo::StFgt2PointCosmicTrackAlgo():mIsInitialized(0) {
  //nothing else to do....
};

// initialize
Int_t StFgt2PointCosmicTrackAlgo::Init() {
  mIsInitialized=true;
  return kStOk;
};


Int_t StFgt2PointCosmicTrackAlgo::makeCosmicTracks(StFgtPointCollection& points, StFgtCosmicTrackVec& tracks, Int_t eventCounter) {

  // calculate track from 2 points on outer discs -> get 4 param for line
  Float_t x_0=0, y_0=0, trackX_0=0, trackY_0=0;
  Float_t a=1, b=1, trackA=1, trackB=1;
  Float_t dX=0, dY=0, dR=0, trackdX=0, trackdY=0, trackdR=99999, hitX=0, hitY=0;
  Bool_t isTrack=false, isTrueTrack=false;
  Float_t XoS=0, YoS=0, ZoS=0, OoS=0,
          XXoS=0, YYoS=0, ZZoS=0, ZXoS=0, ZYoS=0,
          sigma=0.3*0.3;// +a*a*0.5*0.5;// assuming known Z and XY errors of 3mm
  Int_t quad0Id, quad1Id, quad2Id;
  Float_t x0,y0,z0,r0,phi0,x1,y1,z1,r1,phi1,x2,y2,z2,r2,phi2;

  const StSPtrVecFgtPoint &pointVec = points.getPointVec();
  StSPtrVecFgtPointConstIterator pointIter0, pointIter1, pointIter2;

  for( pointIter0 = pointVec.begin(); pointIter0 != pointVec.end(); ++pointIter0 ) {
    quad0Id = (*pointIter0)->getDisc();
    r0   = (*pointIter0)->getPositionR();
    phi0 = (*pointIter0)->getPositionPhi();
    x0   = r0*cos(phi0+.2618);
    y0   = r0*sin(phi0+.2618);
    z0 = 30.48; // 12 inches in cm, top quadrant
    if( quad0Id==0 ) {
      for( pointIter1 = pointVec.begin(); pointIter1 != pointVec.end(); ++pointIter1) {
       quad1Id = (*pointIter1)->getDisc();
       r1   = (*pointIter1)->getPositionR();
       phi1 = (*pointIter1)->getPositionPhi();
       x1   = r1*cos(phi1+.2618);
       y1   = r1*sin(phi1+.2618);
       z1 = 15.24; // 6 inches in cm, middle quadrant
       if( quad1Id==1 ) { 
        for( pointIter2 = pointVec.begin(); pointIter2 != pointVec.end(); ++pointIter2) {
         quad2Id = (*pointIter2)->getDisc();
         r2   = (*pointIter2)->getPositionR();
         phi2 = (*pointIter2)->getPositionPhi();
         x2   = r2*cos(phi2+.2618);
         y2   = r2*sin(phi2+.2618);
         z2 = 0.; // take origin as bottom quadrant
         if( quad2Id==2 ) {
          XoS = (x0+x1+x2)/sigma;
          YoS = (y0+y1+y2)/sigma;
          ZoS = (z0+z1+z2)/sigma;
          OoS = 3/sigma;
          XXoS = (x0*x0+x1*x1+x2*x2)/sigma;
          YYoS = (y0*y0+y1*y1+y2*y2)/sigma;
          ZZoS = (z0*z0+z1*z1+z2*z2)/sigma;
          ZXoS = (x0*z0+x1*z1+x2*z2)/sigma;
          ZYoS = (y0*z0+y1*z1+y2*z2)/sigma;

          // do two 2D cases. x = a*z + x_0 and y = b*z + y_0
          Float_t denominator = ZZoS*OoS-ZoS*ZoS;
          if(denominator != 0) {
            a   = (ZXoS*OoS-XoS*ZoS)/denominator;
            x_0 = (ZZoS*XoS-ZXoS*ZoS)/denominator;
            b   = (ZYoS*OoS-YoS*ZoS)/denominator;
            y_0 = (ZZoS*YoS-ZYoS*ZoS)/denominator;
          }

          // look at middle disc -> extrapolate expected point from line -> does data fit expectation within a region pi*r^2? (use r=0.3cm)
          dX = x1-(a*z1+x_0);
          dY = y1-(b*z1+y_0);
          dR=sqrt(dX*dX + dY*dY);
          if(dR<0.3) {
            //cout << "\nFound track in event " << eventCounter << "! (2-point fit)" << endl;
            isTrack = true;
          }
          
          // store track with smallest dR
          if(dR<trackdR) {
            trackdR=dR;
            trackA=a;
            trackB=b;
            trackX_0=x_0;
            trackY_0=y_0;
            trackdX=dX;
            trackdY=dY;
            hitX=x1;
            hitY=y1;
            isTrueTrack=isTrack;
          }
         }
        }
       }
      }
    }
  }

  // now must store values and a,b,x_0,y_0,dX,dY,dR in a container in StFgtEvent
  // for now -> store dR instead of chi2 val

  tracks.push_back( StFgtCosmicTrack (eventCounter, trackA, trackB, trackX_0, trackY_0, trackdX, trackdY, trackdR, hitX, hitY, isTrueTrack) );

  return kStOk;
};

ClassImp(StFgt2PointCosmicTrackAlgo);
