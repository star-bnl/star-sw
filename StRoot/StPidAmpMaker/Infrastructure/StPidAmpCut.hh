/***************************************************************************
 *
 * $Id: StPidAmpCut.hh,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
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
         ~StPidAmpCut();

         double  lowEdge() const;
         double highEdge() const;
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
inline string StPidAmpCut::name() const {return mName; }

#endif
//in mName, "N" means Nhits cut.
//"P" means Pt cut
//"R" means rigidity cut.
//the compiler can not recognize "bool" without "include <iostream>"
//see "/afs/rhic/star/packages/ObjectSpace/2.0m/ospace/type/prim.h"  for more about bool
