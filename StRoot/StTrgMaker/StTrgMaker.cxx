/***************************************************************************
 *
 * $Id: StTrgMaker.cxx,v 1.4 2001/07/27 17:40:18 ward Exp $
 *
 * Author: Herbert Ward April 2001
 ***************************************************************************
 *
 * Description:  Accepts a .event.root file as input, outputs a .out
 *               containing the CTB slat ADCs and also information
 *               about which slats were hit by tracks.  The .out file
 *               is ASCII; you can parse it               
 *               yourself or use an API from Herb Ward
 *               for reading it.
 *
 ***************************************************************************
 *
 * $Log: StTrgMaker.cxx,v $
 * Revision 1.4  2001/07/27 17:40:18  ward
 * Handles reversed B field, also has code for chking triggerWord.
 *
 * Revision 1.3  2001/07/22 23:00:27  ward
 * Added diagnostics to output file.  Also doc improvements.
 *
 * Revision 1.2  2001/07/17 19:14:38  ward
 * Avoid edges of CTB slats and other wild situations.
 *
 * Revision 1.1  2001/04/23 20:00:27  ward
 * Outputs info for CTB calib: slat ADCs and TPC track extensions.
 *
 *
 **************************************************************************/

#include "StTrgMaker.h"
#include "StEventTypes.h"
#include "TNtuple.h"
#include "TFile.h"
#include "StMessMgr.h"
#define OO fprintf(oo,

#define RPD 0.0174533 // Radians per degree.
#define INNERRADIUS 215.75 /* centimeters, inner slats */
#define OUTERRADIUS 212.58 /* centimeters, outer slats */
#define INNER_OUTER_ZBOUNDARY 112.0
#define DEAD_ZONE 12.0
#define OUTER_ZBOUNDARY       231.1
#define PI 3.1415926
#define ZCUT 50.0 // cm
#define MAXTRKS 500000

ClassImp(StTrgMaker)

StTrgMaker::StTrgMaker(const Char_t *name) : StMaker(name) {
  mEventCounter = 0;
}
StTrgMaker::~StTrgMaker() {
  /* noop */
}
Int_t StTrgMaker::Init() {
  FILE *ff;
  ff=fopen("StTrgMaker.out","w");
  assert(ff); fclose(ff);
  return StMaker::Init();
}
void StTrgMaker::Clear(Option_t *opt) {
  StMaker::Clear();
}
Int_t StTrgMaker::Finish() {
  return kStOK;
}
Int_t StTrgMaker::Make() {
  FILE *out; double curvature,phi0,psi,r0,tanl,z0; long q; int adc;
  out=fopen("StTrgMaker.out","a"); assert(out);
  mEventCounter++;  // increase counter

  StEvent* event;
  event = (StEvent *) GetInputDS("StEvent");
  if (!event) { fclose(out); return kStOK; }
  if(!event->primaryVertex()) {
    fprintf(out,"# Event %3d: has no primary vertex\n",mEventCounter);
    fclose(out); return kStOK;
  }
  StThreeVectorD vertexPos(0,0,0);
  vertexPos=event->primaryVertex()->position();
  printf("Event %3d: the position of the primary vertex is (%g %g %g)\n",
     mEventCounter,vertexPos.x(), vertexPos.y(), vertexPos.z());
  if(vertexPos.z()>50.0||vertexPos.z()<-50.0) {
    fprintf(out,"# Event %3d: the primary vertex is out of z-range: %6.1f.\n",mEventCounter,vertexPos.z());
    fclose(out);
    return kStOK;
  }

  StEventSummary *summary = event->summary();  // This should be in StTrgMaker::Init().  It wastes time 
  assert(summary);                             // every event.  But no
  mMagneticField = summary->magneticField();   // big deal.

  StTriggerDetectorCollection *theTriggers = event->triggerDetectorCollection();
  if (!theTriggers) {
    fprintf(out,"# Event %3d: triggerDetectorCollection is missing\n",mEventCounter);
    fclose(out);
    return kStOK;
  }
  StL0Trigger *l0Trigger = event->l0Trigger();
  if(!l0Trigger) {
    fprintf(out,"# Event %3d: l0Trigger is missing\n",mEventCounter);
    fclose(out);
    return kStOK;
  }
  fprintf(out,"# Event %3d, triggerWord = 0x%04x\n",mEventCounter,l0Trigger->triggerWord());

  StCtbTriggerDetector &theCtb = theTriggers->ctb();

  fprintf(out,"e %d\n",mEventCounter);
  for(unsigned int islat=0; islat<theCtb.numberOfSlats(); islat++) 
    for(unsigned int itray=0; itray<theCtb.numberOfTrays(); itray++) {
      adc=theCtb.mips(itray, islat, 0);
      if(adc) fprintf(out,"a%d:%d %d\n",itray+1,islat+1,adc);
  }

  // Get the tracks and use them.
  StTrack *track; StTrackGeometry *geo;
  StSPtrVecTrackNode& nodes = event->trackNodes();
  StThreeVectorF o,momentum;
  for(unsigned int j=0;j<nodes.size();j++) {
    track = nodes[j]->track(global);
    if(accept(track)) {
      geo=track->geometry();
      o=geo->origin();
      momentum=geo->momentum();
      fprintf(out,"P%g\n",momentum.magnitude());
      fprintf(out,"D%g\n",0.0); // This is supposed to be de/dx, I'm putting in 0.0 as a quick fix.
      q=geo->charge();
      curvature=geo->curvature();
      phi0=o.phi()*180/3.1415926;    // StEvent is in radians, contrary to STAR convention.
      psi=geo->psi()*180/3.1415926;  // StEvent is in radians, contrary to STAR convention.
      r0=sqrt(o.x()*o.x()+o.y()*o.y());
      tanl=geo->dipAngle();
      z0=o.z();
      // fprintf(out,"%2d %2d %g %g %g %g %g %g\n",++BBB,q,curvature,phi0,psi,r0,tanl,z0);
      DoOneTrack(out,q,curvature,phi0,psi,r0,tanl,z0);
    }
  }

  fclose(out); return kStOK;
}
bool StTrgMaker::accept(StTrack* track) {
  return track && track->flag() >= 0;
}
void StTrgMaker::FindIntersectionOfTwoCircles(
     double center1x,double center1y,double radius1,  /* input (circle 1) */
     double center2x,double center2y,double radius2,  /* input (circle 2) */
     int *numIntersectionPoints,                      /* output */
     double *intersection1x,double *intersection1y,   /* output */
     double *intersection2x,double *intersection2y    /* output */
) {

  double tempx,tempy,radicand,transX,transY,angle;

  if(center1x==center2x&&center1y==center2y&&radius1==radius2) { *numIntersectionPoints=0; return; }
  /* PP"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\n"); */
  /* Do a translation: put circle1's center at the origin and circle2's center on the x-axis. */
  transX=-center1x; transY=-center1y; /* Translation necessary to put center of circle 1 at origin. */
  center1x=0; center1y=0; center2x+=transX; center2y+=transY; /* Do translation. */
  angle=-atan2(center2y,center2x); /* Rotation necessary to put center of circle 2 on x-axis. */
  center2x=sqrt(center2x*center2x+center2y*center2y); center2y=0; /* Do rotation. */

  /* The two intersection points have the same x coordinate.  A little simple algebra gives */
  /* this x value and also the two y values (by symmetry, y1 = -y2). */
  *intersection1x=(radius1*radius1-radius2*radius2+center2x*center2x)/(2*center2x);
  /* PP"BBB intersection1x = %g\n",*intersection1x); */
  *intersection2x=*intersection1x;
  radicand=radius1*radius1-(*intersection1x)*(*intersection1x);
  /* PP"BBB radicand = %g\n",radicand); */

       if(radicand< 0) *numIntersectionPoints=0;
  else if(radicand==0) *numIntersectionPoints=1;
  else                 *numIntersectionPoints=2;

  /* PP"BBB nip = %d\n",*numIntersectionPoints); */

  if(*numIntersectionPoints>0) {
    *intersection1y=+sqrt(radicand);
    *intersection2y=-sqrt(radicand);

    /* Now rotate and tranlate the two intersection points back to the original frame. */
    /* PP"BBB angle = %g degrees\n",angle*180/PI); */
    /* PP"BBB trans = %g %g\n",transX,transY); */
    tempx=cos(-angle)*(*intersection1x)-sin(-angle)*(*intersection1y);
    tempy=sin(-angle)*(*intersection1x)+cos(-angle)*(*intersection1y);
    tempx-=transX; tempy-=transY;
    *intersection1x=tempx; *intersection1y=tempy;
    tempx=cos(-angle)*(*intersection2x)-sin(-angle)*(*intersection2y);
    tempy=sin(-angle)*(*intersection2x)+cos(-angle)*(*intersection2y);
    tempx-=transX; tempy-=transY;
    *intersection2x=tempx; *intersection2y=tempy;
  } else {
    *intersection1x=0; *intersection1y=0; *intersection2x=0; *intersection2y=0;
  }

}
void StTrgMaker::CalcCenterOfCircleDefinedByTrack(int q,double radius,double psi,double r0,
      double phi0,double *xcenter,double *ycenter) {
  double angleOffset,xstart,ystart;
  xstart=r0*cos(RPD*phi0);
  ystart=r0*sin(RPD*phi0);
  if(mMagneticField>0) { if(q>=0) angleOffset=-90; else angleOffset= 90; }
  else                 { if(q>=0) angleOffset= 90; else angleOffset=-90; }
  *xcenter=xstart+radius*cos(RPD*(psi+angleOffset));
  *ycenter=ystart+radius*sin(RPD*(psi+angleOffset));
}
void StTrgMaker::DoOneTrack(FILE *oo,long q,double curvature,double phi0,
         double psi,double r0,double tanl,double z0) {
  double xcenter,ycenter,radius;
  char inner,noIntersectionInner=0,noIntersectionOuter=0; /* These are boolean values (T/F). */
  int traynumber,count=0,numberIntersectionPointsInner,numberIntersectionPointsOuter;
  double xintersection,yintersection,zintersection,angle=0;
  double intersectionInner1X,intersectionInner1Y,intersectionInner2X,intersectionInner2Y;
  double intersectionOuter1X,intersectionOuter1Y,intersectionOuter2X,intersectionOuter2Y;
  double xUnitVector,yUnitVector,xstart,ystart,dotProduct1,dotProduct2;
  double innerIntersectionX,innerIntersectionY;
  double outerIntersectionX,outerIntersectionY;

  /* Calculate the radius and center (x,y) of the circle. */
  radius=1/curvature; CalcCenterOfCircleDefinedByTrack(q,radius,psi,r0,phi0,&xcenter,&ycenter);

  /* Find 2 intersection points (of the circle) with the inner slats, and 2 with the outer slats. */
  FindIntersectionOfTwoCircles(0.0,0.0,INNERRADIUS,xcenter,ycenter,radius,&numberIntersectionPointsInner,
          &intersectionInner1X,&intersectionInner1Y,
          &intersectionInner2X,&intersectionInner2Y);
  FindIntersectionOfTwoCircles(0.0,0.0,OUTERRADIUS,xcenter,ycenter,radius,&numberIntersectionPointsOuter,
          &intersectionOuter1X,&intersectionOuter1Y,
          &intersectionOuter2X,&intersectionOuter2Y);

  /*
  PP"r0,phi0,psi %g %g %g\n",r0,phi0,psi);
  PP"xcenter,ycenter,radius %g %g %g\n",xcenter,ycenter,radius);
  PP"# i pts %d %d\n",numberIntersectionPointsInner,numberIntersectionPointsOuter);
  */
  /* Eliminate one of each pair:  the one which represents the second (chronologically) intersection. */
  /* Do this using direction cosines calculated using vector dot products.                            */
  /* It is, of course, not necessary if the "pair" is not a pair                                      */
  /* (numberIntersectionPointsInner<2 or numberIntersectionPointsOuter<2).                            */
  xUnitVector=cos(RPD*psi); yUnitVector=sin(RPD*psi); /* Direction of path at beginning of track. */
  xstart=r0*cos(RPD*phi0); ystart=r0*sin(RPD*phi0);   /* This is (x,y) at beginning of track.     */
  /* Do the points for the inner slats first. */
  if(numberIntersectionPointsInner==2) {
    dotProduct1=xUnitVector*(intersectionInner1X-xstart)+yUnitVector*(intersectionInner1Y-ystart);
    dotProduct1/=sqrt((intersectionInner1X-xstart)*(intersectionInner1X-xstart)+
                      (intersectionInner1Y-ystart)*(intersectionInner1Y-ystart)); /* Normalize. */
    dotProduct2=xUnitVector*(intersectionInner2X-xstart)+yUnitVector*(intersectionInner2Y-ystart);
    dotProduct2/=sqrt((intersectionInner2X-xstart)*(intersectionInner2X-xstart)+
                      (intersectionInner2Y-ystart)*(intersectionInner2Y-ystart)); /* Normalize. */
    if(dotProduct1>=dotProduct2) {
      innerIntersectionX=intersectionInner1X; innerIntersectionY=intersectionInner1Y;
    } else {
      innerIntersectionX=intersectionInner2X; innerIntersectionY=intersectionInner2Y;
    }
  } else if(numberIntersectionPointsInner==1) {
    innerIntersectionX=intersectionInner1X; innerIntersectionY=intersectionInner1Y;
  } else if(numberIntersectionPointsInner==0) {
    noIntersectionInner=7;
  } else assert(0);
  /* Now do the points for the outer slats. */
  if(numberIntersectionPointsOuter==2) {
    dotProduct1=xUnitVector*(intersectionOuter1X-xstart)+yUnitVector*(intersectionOuter1Y-ystart);
    dotProduct1/=sqrt((intersectionOuter1X-xstart)*(intersectionOuter1X-xstart)+
                      (intersectionOuter1Y-ystart)*(intersectionOuter1Y-ystart)); /* Normalize. */
    dotProduct2=xUnitVector*(intersectionOuter2X-xstart)+yUnitVector*(intersectionOuter2Y-ystart);
    dotProduct2/=sqrt((intersectionOuter2X-xstart)*(intersectionOuter2X-xstart)+
                      (intersectionOuter2Y-ystart)*(intersectionOuter2Y-ystart)); /* Normalize. */
    if(dotProduct1>=dotProduct2) {
      outerIntersectionX=intersectionOuter1X; outerIntersectionY=intersectionOuter1Y;
    } else {
      outerIntersectionX=intersectionOuter2X; outerIntersectionY=intersectionOuter2Y;
    }
  } else if(numberIntersectionPointsOuter==1) {
    outerIntersectionX=intersectionOuter1X; outerIntersectionY=intersectionOuter1Y;
  } else if(numberIntersectionPointsOuter==0) {
    noIntersectionOuter=7;
  } else assert(0);

  /* At this point we have q,invp,curvature,length,phi0,psi,r0,tanl,z0, */
  /* xstart,ystart,xcenter,ycenter,radius,                              */
  /* noIntersectionInner,innerIntersectionX,innerIntersectionY,         */
  /* noIntersectionOuter,outerIntersectionX,outerIntersectionY          */

  /* Now we use tanl to set xintersection, yintersection, and zintersection. */
  /* After we do this, the variables                                         */
  /*     noIntersectionInner,innerIntersectionX,innerIntersectionY,          */
  /*     noIntersectionOuter,outerIntersectionX,outerIntersectionY           */
  /* will be obsolete.                                                       */
  /* The variable angle is in radians, contrary to STAR convention.          */
  if(!noIntersectionInner) {
    angle+=atan2(innerIntersectionY-ycenter,innerIntersectionX-xcenter)-atan2(ystart-ycenter,xstart-xcenter);
    count++;
  }
  if(!noIntersectionOuter) {
    angle+=atan2(outerIntersectionY-ycenter,outerIntersectionX-xcenter)-atan2(ystart-ycenter,xstart-xcenter);
    count++;
  }
  // The 180 below does this. if(fabs(tanl)>0.8) { FakeInfo(oo,101); return; }
  if(count<1) { FakeInfo(oo,124); return; } /* The track does not intersect the CTB because of low pt. */
  angle/=count; zintersection=z0+tanl*radius*fabs(angle);
  if(fabs(zintersection)>180.0) { FakeInfo(oo,107); return; }
  if(fabs(zintersection)<INNER_OUTER_ZBOUNDARY-DEAD_ZONE) {
    inner=7; if(noIntersectionInner) { FakeInfo(oo,121); return; }
    xintersection=innerIntersectionX; yintersection=innerIntersectionY;
  } else if(fabs(zintersection)>INNER_OUTER_ZBOUNDARY+DEAD_ZONE&&fabs(zintersection)<=OUTER_ZBOUNDARY) {
    inner=0;
    xintersection=outerIntersectionX; yintersection=outerIntersectionY;
    if(noIntersectionOuter) { FakeInfo(oo,122); return; }
  } else { FakeInfo(oo,123); return; /* Return if the track left via the endcap. */ }

  /* Now we can determine the slat number. */
  traynumber=TrayNumber(xintersection,yintersection,zintersection);
  if(traynumber<1) { FakeInfo(oo,129); return; }

  /* Output. */
  OO"X%1.1f\n",xintersection);
  OO"Y%1.1f\n",yintersection);
  OO"Z%1.1f\n",zintersection);
  OO"S%d:%d\n",traynumber,inner?1:2);

  /* This debugging section is commented out for production. --------------------
  PP"q=%2d invp=%5.2f curv=%8.6f len=%4.0f phi0=%3.0f psi=%3.0f r0=%3.0f tanl=%9.5f z0=%4.0f i/c=%5.2f\n",
     q,invp,curvature,length,phi0,psi,r0,tanl,z0,invp/curvature);
  PP"start point = %6.1f %6.1f\n",xstart,ystart);
  PP"center of circle = (%6.1f %6.1f), radius=%6.1f\n",xcenter,ycenter,radius);
  PP"plot 0 0 216 0 0 213 %6.1f %6.1f %6.1f\n",xcenter,ycenter,radius);
  PP"noIntersectionInner=%d, innerIntersectionX=%6.1f, innerIntersectionY=%6.1f\n",
      noIntersectionInner,innerIntersectionX,innerIntersectionY);
  PP"noIntersectionOuter=%d, outerIntersectionX=%6.1f, outerIntersectionY=%6.1f\n",
      noIntersectionOuter,outerIntersectionX,outerIntersectionY);
  PP"angle from start point to intersection is %6.1f degrees.\n",angle/RPD);
  PP"The intersection point is %6.1f %6.1f %6.1f\n",xintersection,yintersection,zintersection);
  -------------------------------------------------------------------------------*/
}
/*
FILE *oo,
long q,
double invp,
double curvature,
double length,
double phi0,
double psi,
double r0,
double tanl,
double z0,
*/
#define FIDUCIAL 0.4 // This tells how much of the slat on either side of the centerline
                     // to accept as fiducial.  Thus, 0.5 would use the "entire" slat.
                     // If you want to reject tracks which strike close to the edge, make
                     // it a little smaller, say 0.4.
int StTrgMaker::TrayNumber(double x,double y,double z) {
  double tv,angle; int rv;
  angle=atan2(y,x);
  while(angle< 0.0) angle+=2*PI;
  while(angle>2*PI) angle-=2*PI;
  angle*=60/(2*PI); /* Var "angle" is in units of 1/60th of circle, ccw from x-axis. */
  if(z>0) {
    tv=120-angle+13.5; /* "120" avoids probs w double -> int conversion of negative doubles (next line). */
    rv=tv;
    if(fabs((double)(tv-rv-0.5))>FIDUCIAL) return -1; /* Track is not close enough to centerline of slat. */
    while(rv<  1) rv+=60; while(rv> 60) rv-=60;
  } else {
    tv=120+angle+103.5;  /* "120" avoids probs w double -> int conversion of negative doubles (next line). */
    rv=tv;
    if(fabs((double)(tv-rv-0.5))>FIDUCIAL) return -1; /* Track is not close enough to centerline of slat. */
    while(rv< 61) rv+=60; while(rv>120) rv-=60;
  }
  return rv;
}
void StTrgMaker::FakeInfo(FILE *oo,int code) {
  OO"X0  # %d\n",code);
  OO"Y0\n");
  OO"Z0\n");
  OO"S-123:1\n");
}

