/***************************************************************************
 *
 * $Id: StPidAmpSliceInfo.hh,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpSliceInfo contains fitted information of a slice.
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpSliceInfo.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


//fitting results of slice.
#ifndef StPidAmpSliceInfo_hh
#define StPidAmpSliceInfo_hh

#include <iostream.h>

class StPidAmpSliceInfo {

public:

      StPidAmpSliceInfo();
      StPidAmpSliceInfo(double meanDedx, double sigma, double amp);
      ~StPidAmpSliceInfo();
      
      double  meanDedx() const;
      double     sigma() const;
      double       amp() const;
      void    setMeanDedx(const double& dedx);
      void    setSigma(const double& sig);
      void    setAmp(const double& amp);
      void    setInfo(const double& d,const double& s,const double& a); 
       //call setMeanDedx() etc.


private:

      double mMeanDedx;
      double mSigma;
      double mAmp;
};

ostream& operator<<(ostream& s, const StPidAmpSliceInfo& info);

inline double StPidAmpSliceInfo::meanDedx() const {return mMeanDedx;}
inline double StPidAmpSliceInfo::sigma() const {return mSigma;}
inline double StPidAmpSliceInfo::amp() const {return mAmp;}
inline void  StPidAmpSliceInfo::setMeanDedx(const double& dedx){mMeanDedx=dedx;}
inline void  StPidAmpSliceInfo::setSigma(const double& sig){mSigma=sig;}
inline void  StPidAmpSliceInfo::setAmp(const double& amp){mAmp=amp;}
inline void  StPidAmpSliceInfo::setInfo(const double&d, const double& s, const double& a)
{setMeanDedx(d); setSigma(s); setAmp(a);}


#endif
