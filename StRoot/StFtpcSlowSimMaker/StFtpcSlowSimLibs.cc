// $Id: StFtpcSlowSimLibs.cc,v 1.2 2003/09/02 17:58:16 perev Exp $
// $Log: StFtpcSlowSimLibs.cc,v $
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

    if (    rising  && (xx > x[jup] || xx < x[jlow])  ||
          (!rising) && (xx < x[jup] || xx > x[jlow]) ) {
         cout << "Locate(): xx is out of range!" << endl;
         return 0;
    }

    while (jup-jlow-1 ) {
        jmid = (jlow + jup)/2;

        if (    rising  && (xx > x[jmid])  ||
              (!rising) && (xx < x[jmid]) ) {
            jlow = jmid;
        }
        else {
            jup  = jmid;
        }
    }

    return jlow;
}

