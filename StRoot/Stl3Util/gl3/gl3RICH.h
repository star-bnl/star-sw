//:>------------------------------------------------------------------
//: FILE:       gl3RICH.h
//: HISTORY:
//:             22Jun2K version 1.00
//:<------------------------------------------------------------------
#include "Stl3Util/gl3/gl3Algorithm.h"

#ifndef GL3RICH   
#define GL3RICH  

#ifdef GL3ROOT

#include "TH2.h"
#include "TNtuple.h"
#include "TProfile.h"
 
#endif   



class gl3RICH: public gl3Algorithm {

public:
    virtual int init    (  ) ;

    virtual int setParameters(int maxAbsVertz, int minNoOfHits, int are, int place , int holders, float minP, float minPt, float minR, float maxDCA, float minimumAbsEta );

    virtual int decide ( ) ;
    virtual int end     (  ) ;

    virtual const int   getAlgorithmID() 
	{ return L3_ALGORITHM_RICH; }
    virtual const char *getAlgorithmName() 
	{ return L3_ALGORITHM_RICH_NAME; }
    

private:
  double mGlobalRichEdgeXmin;
  double mGlobalRichEdgeXmax;
  double mGlobalRichEdgeYmin;
  double mGlobalRichEdgeYmax;
  double mGlobalRichEdgeZmin;
  double mGlobalRichEdgeZmax;

  // Cylindrical coordinates: 5 o'clock = -60 degrees
  double mLocalOriginR;
  double mLocalOriginPhi;
    
  double maxAbsEventVertZ;
  int    minNoOfHitsOnTrack;
  double minPExtrapolated2Rich;
  double minPtExtrapolated2Rich;
  double minRofLastPointOnTrack;
  double maxDCAToEventVertex;
  double minAbsEta;
  
  

#ifdef GL3ROOT
  TH1D* hPtGlobalPosRICH;
  TH1D* hPtGlobalNegRICH;

  TH1D* hPGlobalPosRICH;
  TH1D* hPGlobalNegRICH;

  TH1D* hnHitsGlobalPosRICH;
  TH1D* hnHitsGlobalNegRICH;

  TH1D* hXVertex;
  TH1D* hYVertex;
  TH1D* hZVertex;

  TH1D* hdcaGlobal;
  TH1D* hetaRICH;
  TH1D* hRLast;
#endif

};
#endif
