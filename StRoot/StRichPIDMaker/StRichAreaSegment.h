/**********************************************************
 * $Id: StRichAreaSegment.h,v 2.1 2000/11/21 16:24:22 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichAreaSegment.h,v $
 *  Revision 2.1  2000/11/21 16:24:22  horsley
 *  Major overhaul of StRichArea, introduced monte carlo integration cross check,
 *  all possible areas, angles calculated together. StRichRingCalculator, StRichPIDMaker modified to support new StRichArea. StRichPIDMaker's hit finder
 *  typo corrected.
 *
 *  Revision 2.1  2000/09/29 01:35:36  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.2  2000/05/19 19:06:10  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:07  horsley
 *  initial revision
**********************************************************/

#ifndef StRichAreaSegment_H
#define StRichAreaSegment_H

#include "StThreeVectorF.hh"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif


class StRichAreaSegment {

public:
  StRichAreaSegment();
  ~StRichAreaSegment();
  StThreeVectorF& getPoint(int);
  void            addPoint(int, StThreeVectorF);
  double          getAngle(int);
  void            addAngle(int,double);
  void            setType(int);
  int             getType();

private:
  vector<StThreeVectorF> mPoints; //!
  vector<double> mAngle; //!
  int mType;
  double mAngle_0,mAngle_1;

};

#endif
