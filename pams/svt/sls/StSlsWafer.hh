#ifndef STSLSWAFER_HH
#define STSLSWAFER_HH
#include <stdiostream.h>
#include <math.h>
#include "StSlsListStrip.hh"
#include "StSlsListPoint.hh"

class StSlsWafer
{
 public:
                  StSlsWafer(int id);
                  ~StSlsWafer();

  void            init(int rId, float *rD, float *rT, float *rN, float *rX);
  void            addHit(int rNId , int rMcHit, int rMcTrack, float *rXg , float rDe, float *p);
  int             convertGlobalToLocal();
  int             convertLocalToUFrame(float ActiveLargeEdge, float ActiveSmallEdge, float Theta);
  StSlsListPoint* getDeadHits(float ActiveLargeEdge, float ActiveSmallEdge,float Test);
  void            convertToStrip(float Pitch, 
				 int nStripPerSide,
				 double PairCreationEnergy,
				 int NStripInACluster,
				 double ParDiffP,
				 double ParDiffN,
				 double ParIndRightP,
				 double ParIndRightN,
				 double ParIndLeftP,
				 double ParIndLeftN);

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
				    int    NStripInACluster,
				    double ParDiffP,
				    double ParDiffN,
				    double ParIndRightP,
				    double ParIndRightN,
				    double ParIndLeftP,
				    double ParIndLeftN);
  void            convertAnalogToDigit(double PairCreationEnergy);
  float*          findAngle(float *p, float *alpha);
};  
#endif
