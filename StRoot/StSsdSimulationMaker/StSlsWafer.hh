// $Id: StSlsWafer.hh,v 1.3 2006/09/15 21:09:52 bouchet Exp $
//
// $Log: StSlsWafer.hh,v $
// Revision 1.3  2006/09/15 21:09:52  bouchet
// read the noise and pedestal from ssdStripCalib
//
// Revision 1.2  2005/05/13 08:39:32  lmartin
// CVS tags added
//

#ifndef STSLSWAFER_HH
#define STSLSWAFER_HH
#include <math.h>
#include "StSlsListStrip.hh"
#include "StSlsListPoint.hh"
#include "Rtypes.h"
class StSlsWafer
{
 public:
                  StSlsWafer(int id);
                  ~StSlsWafer();

  void            init(int rId, Double_t *rD, Double_t *rT, Double_t *rN, Double_t *rX);
  void            addHit(int rNId , int rMcHit, int rMcTrack, float *rXg , float rDe, float *p);
  int             convertGlobalToLocal();
  int             convertLocalToUFrame(float ActiveLargeEdge, float ActiveSmallEdge, float Theta);
  StSlsListPoint* getDeadHits(float ActiveLargeEdge, float ActiveSmallEdge,float Test);
  void            convertToStrip(float Pitch, 
				 int nStripPerSide,
				 double pairCreationEnergy,
				 int nstripInACluster,
				 double parDiffP,
				 double parDiffN,
				 double parIndRightP,
				 double parIndRightN,
				 double parIndLeftP,
				 double parIndLeftN);

  int             getId();
  float*          getD();
  float*          getT();
  float*          getN();
  float*          getX();
  StSlsListPoint* getPoint();
  StSlsListStrip* getStripP();
  StSlsListStrip* getStripN();

private:
  int             mId;
  float          *mD;
  float          *mT;
  float          *mN;
  float          *mX;
  StSlsListPoint *mPoint;
  StSlsListStrip *mStripP;
  StSlsListStrip *mStripN;

  StSlsListPoint* getNonActivePointBorder(float ActiveLargeEdge, float ActiveSmallEdge);
  StSlsListPoint* getNonActivePointTriangle(float Test);
  double          myErf(double x);
  void            convertHitToStrip(float Pitch, int nStripPerSide,
				    int    nstripInACluster,
				    double parDiffP,
				    double parDiffN,
				    double parIndRightP,
				    double parIndRightN,
				    double parIndLeftP,
				    double parIndLeftN);
  void            convertAnalogToDigit(double pairCreationEnergy);
  float*          findAngle(float *p, float *alpha);
};  
#endif
