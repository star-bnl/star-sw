/***************************************************************************
 *
 * $Id: StPidAmpCut.hh,v 1.3 2000/05/01 16:59:26 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             Be used by StPidAmpChannelInfo and StPidAmpWindow
 *             In mName, 
 *             "N" means Nhits cut.
 *             "P" means Pt cut
 *             "R" means rigidity cut.         
 ***************************************************************************
 *
 * $Log: StPidAmpCut.hh,v $
 * Revision 1.3  2000/05/01 16:59:26  aihong
 * clean up
 *
 * Revision 1.2  2000/04/09 16:16:34  aihong
 * change for adapting NHitsDcaNet added
 *
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpCut_hh
#define StPidAmpCut_hh

#include <string>

#include <iostream.h>

#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif



class StPidAmpCut {

public:

         StPidAmpCut();
         StPidAmpCut(string s, double low, double high);
         virtual ~StPidAmpCut();

         double  lowEdge() const;
         double highEdge() const;
         double midPoint() const;
         string     name() const;
        
          
         double length();
           void setCut(double low, double high);
           bool isInCut(double x);
           void enable();
           void disable();

private:


        string mName;
        double  mLowEdge;
        double  mHighEdge;
        bool    mBeActive;
};

inline double StPidAmpCut::lowEdge() const { return mLowEdge;}
inline double StPidAmpCut::highEdge() const {return mHighEdge;}
inline double StPidAmpCut::midPoint() const {return (mHighEdge-mLowEdge)*0.5+mLowEdge ;}

inline string StPidAmpCut::name() const {return mName; }

#endif
//in mName, "N" means Nhits cut.
//"P" means Pt cut
//"R" means rigidity cut.
//"D" means dca cut. (distance of closest approach)
//the compiler can not recognize "bool" without "include <iostream>"
//see "/afs/rhic/star/packages/ObjectSpace/2.0m/ospace/type/prim.h"  for more about bool
