#ifndef STSLSPOINT_HH
#define STSLSPOINT_HH

class StSlsPoint
{
 public:
  StSlsPoint(int rNId , int rMcHit, int rMcTrack, float *rXg , float rDe, float *rAngle);
  ~StSlsPoint();

  void        setNId(int rNId);
  void        setMcHit(int rMcHit);
  void        setMcTrack(int rMcTrack);
  void        setXl(float rXl, int iR);
  void        setXg(float rXg, int iR);
  void        setUpos(float rUpos, int iR);
  void        setAngle(float rAngle, int iR);
  void        setDe(float rDe);
  void        setPrevPoint(StSlsPoint *rPrevPoint);
  void        setNextPoint(StSlsPoint *rNextPoint);
  int         getNId();
  int         getMcHit();
  int         getMcTrack();
  float       getXl(int iR);
  float       getXg(int iR);
  float       getUpos(int iR);
  float       getAngle(int iR);
  float       getDe();
  StSlsPoint* getPrevPoint();
  StSlsPoint* getNextPoint();
  StSlsPoint* giveCopy();
  
 private:
  int         mNId;
  int         mMcHit;
  int         mMcTrack;
  float      *mXl;
  float      *mXg;
  float      *mUpos;
  float      *mAngle;
  float       mDe;
  StSlsPoint *mPrevPoint;
  StSlsPoint *mNextPoint;
  
};
#endif
