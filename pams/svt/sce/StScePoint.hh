#ifndef STSCEPOINT_HH
#define STSCEPOINT_HH
#include <stdiostream.h>
#include <stdlib.h>
#include <math.h>

class StScePoint
{
 public:
  StScePoint (int rPoint, int rIdMcHit, int rIdMcTrack, int rIdWaf, float *rXg, float rDe, float *rAngle);
  StScePoint(int rFlag , int rPoint, int rIdCluster, int rIdGlobTrk, int rIdMatch, int *rIdMcHit, int *rIdMcTrack, int *rIdTrack, int rIdWaf, float *rCov , float *rRes, float *rXg, float *rXl, float *rMom2, float *rDe);
  ~StScePoint();
  void        setFlag(int rFlag);
  void        setNPoint(int rPoint);
  void        setIdCluster(int rIdCluster);
  void        setIdGlobTrk(int rIdGlobTrk);
  void        setIdMatch(int rIdMatch);
  void        setIdMcHit(int rIdMcHit, int iR);
  void        setIdMcTrack(int rIdMcTrack, int iR);
  void        setIdTrack(int rIdTrack, int iR);
  void        setIdWaf(int rIdWaf);
  void        setCov(float rCov, int iR);
  void        setRes(float rRes, int iR);
  void        setXg(float rXg, int iR);
  void        setXl(float rXl, int iR);
  void        setMom2(float rMom2, int iR);
  void        setDe(float rDe, int iR);
  void        setUpos(float rUpos, int iR);
  void        setAngle(float rAngle, int iR);

  void        setPrevPoint(StScePoint *rPrevPoint);
  void        setNextPoint(StScePoint *rNextPoint);

  int         getFlag();
  int         getNPoint();
  int         getIdCluster();
  int         getIdGlobTrk();
  int         getIdMatch();
  int         getIdMcHit(int iR);
  int         getIdMcTrack(int iR);
  int         getIdTrack(int iR);
  int         getIdWaf();
  float       getCov(int iR);
  float       getRes(int iR);
  float       getXg(int iR);
  float       getXl(int iR);
  float       getMom2(int iR);
  float       getDe(int iR);
  float       getUpos(int iR);
  float       getAngle(int iR);

  StScePoint* getPrevPoint();
  StScePoint* getNextPoint();

  StScePoint* giveCopy();
  
 private:
  int         mFlag;
  int         mPoint;
  int         mIdCluster;
  int         mIdGlobTrk;
  int         mIdMatch;
  int        *mIdMcHit;
  int        *mIdMcTrack;
  int        *mIdTrack;
  int         mIdWaf;
  float      *mCov;
  float      *mRes;
  float      *mXg;
  float      *mXl;
  float      *mMom2;
  float      *mDe;
  float      *mUpos;
  float      *mAngle;

  StScePoint *mPrevPoint;
  StScePoint *mNextPoint;
};
#endif
