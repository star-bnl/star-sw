/***************************************************************************
 *
 * $Id: StPidAmpTrk.hh,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             class of track for Probability(amplitude) PID
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpTrk.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpTrk_hh
#define StPidAmpTrk_hh

class StPidAmpTrk{

public:

      StPidAmpTrk();
      StPidAmpTrk(double& rig, double& dedx, int& charge,double& pt, int& nhits, double& dca);
      ~StPidAmpTrk();

      double    rig() const;
      double   dedx() const;
      double     pt() const;
      int     nhits() const;
      int    charge() const; //in units of e. although it is double.
      double    dca() const;

private:

       double mRig;
       double mDedx;
       double mPt;
       int    mNhits;
       int    mCharge;
       double mDca;

};

inline double StPidAmpTrk::rig()    const {return mRig;}
inline double StPidAmpTrk::dedx()   const {return mDedx;}
inline int    StPidAmpTrk::charge() const {return mCharge;}
inline double StPidAmpTrk::pt()     const {return mPt;}
inline int    StPidAmpTrk::nhits()  const {return mNhits;}
inline double StPidAmpTrk::dca()    const {return mDca;}

#endif

      
