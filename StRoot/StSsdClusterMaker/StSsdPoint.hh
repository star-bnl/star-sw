#ifndef STSSDPOINT_HH
#define STSSDPOINT_HH

class StSsdPoint
{
 public:
  StSsdPoint(int rNPoint, int rNWafer, int rNumPackage, int rKindPackage);
  StSsdPoint(const StSsdPoint & originalPoint);
  ~StSsdPoint();

  StSsdPoint& operator=(const StSsdPoint originalPoint);

  void        setFlag(int rFlag);
  void        setNPoint(int rNPoint);
  void        setNCluster(int rNCluster);
  void        setNMatched(int rNMatched);
  void        setNMchit(int rNMchit, int iR);
  void        setNWafer(int rNWafer);
  void        setEnergyLoss(float adcP, float adcN);
  void        setDe(float rEnergyLoss, int iR);
  void        setPositionU(float rPositionU, int iR);
  void        setXg(float rXg, int iR);
  void        setXl(float rXl, int iR);

  void        setPrevPoint(StSsdPoint *rPrevPoint);
  void        setNextPoint(StSsdPoint *rNextPoint);

  int         getFlag();
  int         getNPoint();
  int         getNCluster();
  int         getNMatched();
  int         getNMchit(int iR);
  int         getNWafer();
  float       getDe(int iR);
  float       getPositionU(int iR);
  float       getXg(int iR);
  float       getXl(int iR);

  StSsdPoint* getPrevPoint();
  StSsdPoint* getNextPoint();  

  StSsdPoint* giveCopy();
  
 private:

  int         mFlag;
  int         mNPoint;
  int         mNCluster;
  int         mNMatched;
  int        *mNMchit;
  int         mNWafer;
  float      *mDe;
  float      *mPositionU;
  float      *mXg;
  float      *mXl;

  StSsdPoint *mPrevPoint;
  StSsdPoint *mNextPoint;
};

#endif
