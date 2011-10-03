/***************************************************************************
 *
 * $Id: StPidAmpSlice.hh,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             Verticle slice along rigidity(p/z)
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpSlice.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpSlice_hh
#define StPidAmpSlice_hh

#include <iostream.h>
#include <math.h>
#include <string>

#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif


#include "TH1.h"
#include <strstream.h>

#include "StPidAmpSliceInfo.hh"
#include "StPidAmpTrk.hh"
#include "StPidAmpParticle.hh"

//all geo. parameters should be positive.
class StPidAmpSlice {

public:

  StPidAmpSlice();

  StPidAmpSlice(int idx, double meanRig, double lowBound, double highBound, double width, string aNetName, StPidAmpParticle* particleType=0);
  //if slice is in the merging area, particleType=0

  StPidAmpSlice(const StPidAmpSlice& );
  ~StPidAmpSlice();

  void fill(StPidAmpTrk *trk, double x); //fill histogram

  void fit();
  void fillReonstructedSlice(StPidAmpSliceInfo* sliceInfo);
  TH1D* produceDiffSlice();
  StPidAmpSliceInfo* sliceInfo() const;

  void setIndex(int& i);
  void setMeanRig(double& x);
  void setLowBound(double& x);
  void setHighBound(double& x);
  void setWidth(double& x);
  void setParticleType(StPidAmpParticle* particleType);
  void usePathFitResult();
  void setSlice(int& idx,double& meanRig, double& lowBound, double& highBound,double& width);

  int             index() const;
  double        meanRig() const;
  double       lowBound() const;
  double      highBound() const;
  bool isPathFitResUsed() const;
  double       leftEdge() const;
  double      rightEdge() const;
  double       midBound() const;
  double          width() const;
  string           name() const;

  StPidAmpParticle* particleType();
              TH1D* slice();
              TH1D* pathFittedSlice();
              TH1D* reconstructedSlice();
              TH1D* diffSlice();




  

private:

  string    mName; //net::name()+meanRig
  int       mIndex;
  bool      mUsePathFitResult;
  double    mLowBound; //lowest position at mMeanRig
  //note that bounds is a bethe-block curve, mLowBound is the value of that
  //curve at x=mMeanRig. 
  double    mHighBound; //highest position at mMeanRig
  double    mMidBound; //mean default dedx,=mid between mLowBound and mHightBound.
  double    mMeanRig; //mid. between mLeftEdge and mRightEdge
  double    mLeftEdge;
  double    mRightEdge;
  double    mWidth;

  TH1D*     mSlice;
  TH1D*     mPathFittedSlice;
  TH1D*     mReconstructedSlice;
  TH1D*     mDiffSlice;

  StPidAmpParticle*  mParticleType;
  StPidAmpSliceInfo* mSliceInfo;

  

};

ostream& operator<<(ostream& s, const StPidAmpSlice& slice);

inline StPidAmpSliceInfo* StPidAmpSlice::sliceInfo() const
{return mSliceInfo;}
inline void StPidAmpSlice::setIndex(int& i){ mIndex=i;}
inline void StPidAmpSlice::setMeanRig(double& x){mMeanRig=fabs(x);}
inline void StPidAmpSlice::setLowBound(double& x){mLowBound=x;}
inline void StPidAmpSlice::setHighBound(double& x){mHighBound=x;}
inline void StPidAmpSlice::setWidth(double& x){mWidth=x;}
inline void StPidAmpSlice::setParticleType(StPidAmpParticle* particleType){
 mParticleType=particleType;
}


inline void StPidAmpSlice::usePathFitResult(){mUsePathFitResult=true;}

inline void StPidAmpSlice::setSlice(int& idx,double& meanRig, double& lowBound, double& highBound,double& width)
{ setIndex(idx);
  setMeanRig(meanRig);
  setLowBound(lowBound);
  setHighBound(highBound);
  setWidth(width);
  mMidBound =lowBound+(fabs(highBound-lowBound))/2.0;
  mLeftEdge =fabs(mMeanRig)-mWidth/2.0;
  mRightEdge=fabs(mMeanRig)+mWidth/2.0;

  mUsePathFitResult=false;

}

inline int     StPidAmpSlice::index()     const {return mIndex;}
inline double  StPidAmpSlice::meanRig()   const {return mMeanRig;}
inline double  StPidAmpSlice::leftEdge()  const {return mLeftEdge;}
inline double  StPidAmpSlice::rightEdge() const {return mRightEdge;}
inline double  StPidAmpSlice::midBound()  const {return mMidBound;}
inline string  StPidAmpSlice::name()      const {return mName;}
inline double  StPidAmpSlice::lowBound()  const {return mLowBound; }
inline double  StPidAmpSlice::highBound() const {return mHighBound;}
inline double  StPidAmpSlice::width()     const {return mWidth;}

inline  StPidAmpParticle* StPidAmpSlice::particleType() {return mParticleType;}
inline  bool              StPidAmpSlice::isPathFitResUsed() const {return mUsePathFitResult;}


inline TH1D*  StPidAmpSlice::slice()              {return mSlice;}
inline TH1D*  StPidAmpSlice::pathFittedSlice()    {return mPathFittedSlice;}
inline TH1D*  StPidAmpSlice::reconstructedSlice() {return mReconstructedSlice;}
inline TH1D*  StPidAmpSlice::diffSlice()          {return mDiffSlice;}



#endif
