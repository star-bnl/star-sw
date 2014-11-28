// $Id: StFtpcSlowSimLibs.cc,v 1.4 2009/11/14 12:42:27 jcs Exp $
// $Log: StFtpcSlowSimLibs.cc,v $
// Revision 1.4  2009/11/14 12:42:27  jcs
// make corrections to avoid warnings which appeared with system upgrade
//
// Revision 1.3  2007/01/15 15:02:20  jcs
// replace printf, cout and gMesMgr with Logger
//
// Revision 1.2  2003/09/02 17:58:16  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.1  2000/11/23 10:16:43  hummler
// New FTPC slow simulator in pure maker form
//
//
///////////////////////////////////////////////////////////////////////////
//  Author: W.G.Gong
//  Email: gong@mppmu.mpg.de
//  Date:  Oct 25, 1996
///////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <math.h>
#include "StMessMgr.h"


int Locate(const int npt, const float* x, const float xx)
{
//
// Locate the index in the array for a given value
// array x should be a monotonical array
//
    int jlow = 0;
    int jup  = npt-1;
    int jmid;
    int rising = (x[npt-1] > x[0]) ? (1) : (0) ;

    if (    (rising  && (xx > x[jup] || xx < x[jlow]))  ||
          (!rising && (xx < x[jup] || xx > x[jlow])) ) {
         LOG_WARN << "Locate(): xx is out of range!" << endm;
         return 0;
    }

    while (jup-jlow-1 ) {
        jmid = (jlow + jup)/2;

        if (    (rising  && (xx > x[jmid]))  ||
              (!rising && (xx < x[jmid])) ) {
            jlow = jmid;
        }
        else {
            jup  = jmid;
        }
    }

    return jlow;
}

