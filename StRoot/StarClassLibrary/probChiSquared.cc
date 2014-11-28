/***************************************************************************
 *
 * $Id: probChiSquared.cc,v 1.3 2003/09/02 17:59:35 perev Exp $
 *
 * Author: Thomas Ullrich, Apr 2000
 ***************************************************************************
 *
 * Description:
 * 
 * Computes the probability that a random variable having a
 * chi2-distribution with N >= 1 degrees of freedom assumes
 * a value which is larger than a given value chi2 >= 0.
 *
 * The algorithm was taken from CERNLIB prob(G100).
 *
 ***************************************************************************
 *
 * $Log: probChiSquared.cc,v $
 * Revision 1.3  2003/09/02 17:59:35  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2000/09/25 19:30:19  ullrich
 * Fixed code to avoid signed/unsigned comparison warning.
 *
 * Revision 1.1  2000/04/06 22:23:32  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <math.h>

double probChiSquared(double x, unsigned int n)
{
    if(x < 0) return 0;

    const unsigned int nmax   = 300;
    const double r1     = 1;
    const double hf     = r1/2;
    const double th     = r1/3;
    const double f1     = 2*r1/9;
    const double c1     = 1.128379167095513;
    const double chipdf = 100;
    const double xmax   = 174.673;
    const double xmax2  = 2*xmax;
    const double xlim   = 24;
    const double eps    = 1e-30;

    int    m, i;
    double h, w, s, t, fi, e;
    double u = hf*x;

    if (x == 0 || n/20 > x) 
	h=1;
    else if (n == 1) {
	w = ::sqrt(u);
        h = w < xlim ? erfc(w) : 0;
    }
    else if (n > nmax) {
	s = r1/n;
	t = f1*s;
	w = (::pow(x*s,th)-(1-t))/::sqrt(2*t);
	if (w < -xlim) 
	    h=1;
        else if (w < xlim) 
	    h=hf*erfc(w);
        else
	    h=0;
    }
    else {
	m=n/2;
        if(u < xmax2 && (x/n) <= chipdf ) {
	    s=exp(-hf*u);
	    t=s;
	    e=s;
	    if (2*m == (int)n) {
		fi=0;
		for (i=1; i<m; i++) {
		    fi += 1;
		    t=u*t/fi;
		    s=s+t;
		}
		h=s*e;
	    }
	    else {
		fi=1;
		for (i=1; i<m; i++) {
		    fi+=2;
		    t=t*x/fi;
		    s=s+t;
		}
		w=::sqrt(u);
		h = w < xlim ? c1*w*s*e+erfc(w) : 0;
	    }
        }
	else
	    h=0;
    }
    return h > eps ? h : 0;	    
}
