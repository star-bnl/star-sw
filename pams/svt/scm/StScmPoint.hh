#ifndef STSCMPOINT_HH
#define STSCMPOINT_HH

class StScmPoint
{
 public:
  StScmPoint(int rNPoint, int rNWafer, int rNumPackage, int rKindPackage);
  StScmPoint(const StScmPoint & originalPoint);
  ~StScmPoint();

  StScmPoint& operator=(const StScmPoint originalPoint);

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

  void        setPrevPoint(StScmPoint *rPrevPoint);
  void        setNextPoint(StScmPoint *rNextPoint);

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

  StScmPoint* getPrevPoint();
  StScmPoint* getNextPoint();  

  StScmPoint* giveCopy();
  
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

  StScmPoint *mPrevPoint;
  StScmPoint *mNextPoint;
};

#endif
