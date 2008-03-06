#ifndef GL3LMTVERTEXFINDER
#define GL3LMTVERTEXFINDER

#define MAXSLATS 240 
#define MAXTRACK 5000 

#include "gl3Histo.h"
#include "FtfBaseHit.h"
#include "gl3Track.h"

class gl3Event;

class gl3LMVertexFinder {
    
 public:
    gl3LMVertexFinder(int   _minHitsOnTrack = 15,
		      int   _minCTBadc = 3,
		      float _maxZdca = 100.,
		      float _zMargin = 10.,
		      float _phiMargin = 3.0,
		      float _delVertMax = 3.0);
    ~gl3LMVertexFinder();
    
    int setParameters(int   _minHitsOnTrack,
		      int   _minCTBadc,
		      float _maxZdca,
		      float _zMargin,
		      float _phiMargin,
		      float _delVertMax);
    int setParameters(const char *filename);

    int registerHistos();

    int makeVertex(gl3Event *event);
    Ftf3DHit& getVertex()
	{ return myVert; };
    
 private:
    int init();


    gl3Event *event;
    
    int totInpEve;
    static const float C_PI;
    struct JctbHit {float phi,adc,z; int slatIndex;} ctbH[MAXSLATS];
    int NctbH;
    int getCtbHits (gl3Event *);
    void setCtbMap();
    int ctbMap[120][2];
    float zSlats[2][2] ;
    int matchTrack2Ctb( Ftf3DHit *);
    int ppLMV4b( Ftf3DHit *);
    
    gl3Track *trackL[MAXTRACK];
    float ZdcaHitL[MAXTRACK];
    float XdcaHitL[MAXTRACK];
    float YdcaHitL[MAXTRACK];
    int NtrackL;
    
    float CtbRadius;
    float CtbLen2;
    
    Ftf3DHit myVert; 
    // cuts=params
    
    int minHitsOnTrack;
    float  maxZdca;
    int minCTBadc;
    float MatchCtbMaxZ;
    float MatchCtbMaxPhi;
    float DelVertMax;
    
    gl3Histo* hjan[32] ;    
    int nhjan;

    gl3Histo* heve[16] ; 
    int neve;    

    gl3Histo* htime[2];

};
#endif


