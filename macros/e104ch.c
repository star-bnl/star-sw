/* Table of constant values */
#include "Riostream.h"
#include "Rtypes.h"
using namespace std;

Double_t fint(Int_t *narg, Float_t *arg,Int_t *nent,Float_t *ent, *table) {
    /* System generated locals */
    Int_t i__1, i__2, i__3;
    Float_t ret_val, r__1;

    /* Local variables */
    static Int_t loca, locb, locc, ndim, lmin, lmax;
    static Float_t h__;
    static Int_t i__, k, n;
    static Float_t x;
    static Int_t index[32], istep, knots, ishift;
    static Float_t weight[32], eta;


/* CERN PROGLIB# E104    FINT            .VERSION KERNFOR  4.02  820723 */
/* ORIG. 09/08/65 CHL. */

/*   INTERPOLATION ROUTINE. AUTHOR C. LETERTRE. */
/*   MODIFIED BY B. SCHORR, 1.07.1982. */

    /* Parameter adjustments */
    --nent;
    --arg;
    --ent;
    --table;

    /* Function Body */
    ret_val = 0.;
    if (*narg < 1 || *narg > 5) {
	return ret_val;
    }
    lmax = 0;
    istep = 1;
    knots = 1;
    index[0] = 1;
    weight[0] = 1.;
    i__1 = *narg;
    for (n = 1; n <= i__1; ++n) {
	x = arg[n];
	ndim = nent[n];
	loca = lmax;
	lmin = lmax + 1;
	lmax += ndim;
	if (ndim > 2) {
	    goto L10;
	}
	if (ndim == 1) {
	    goto L100;
	}
	h__ = x - ent[lmin];
	if (h__ == 0.) {
	    goto L90;
	}
	ishift = istep;
	if (x - ent[lmin + 1] == 0.) {
	    goto L21;
	}
	ishift = 0;
	eta = h__ / (ent[lmin + 1] - ent[lmin]);
	goto L30;
L10:
	locb = lmax + 1;
L11:
	locc = (loca + locb) / 2;
	if ((r__1 = x - ent[locc]) < 0.) {
	    goto L12;
	} else if (r__1 == 0) {
	    goto L20;
	} else {
	    goto L13;
	}
L12:
	locb = locc;
	goto L14;
L13:
	loca = locc;
L14:
	if (locb - loca > 1) {
	    goto L11;
	}
/* Computing MIN */
	i__2 = TMath::Max(loca,lmin), i__3 = lmax - 1;
	loca = TMath::Min(i__2,i__3);
	ishift = (loca - lmin) * istep;
	eta = (x - ent[loca]) / (ent[loca + 1] - ent[loca]);
	goto L30;
L20:
	ishift = (locc - lmin) * istep;
L21:
	i__2 = knots;
	for (k = 1; k <= i__2; ++k) {
	    index[k - 1] += ishift;
/* L22: */
	}
	goto L90;
L30:
	i__2 = knots;
	for (k = 1; k <= i__2; ++k) {
	    index[k - 1] += ishift;
	    index[k + knots - 1] = index[k - 1] + istep;
	    weight[k + knots - 1] = weight[k - 1] * eta;
	    weight[k - 1] -= weight[k + knots - 1];
/* L31: */
	}
	knots <<= 1;
L90:
	istep *= ndim;
L100:
	;
    }
    i__1 = knots;
    for (k = 1; k <= i__1; ++k) {
	i__ = index[k - 1];
	ret_val += weight[k - 1] * table[i__];
/* L200: */
    }
    return ret_val;
} /* fint */


static Int_t c__1 = 1;
Bool_t e104ch(Int_t *na, Float_t *ent, Int_t *nt, Float_t *table) {
    /* Initialized data */
    static Float_t ex[5] = { -.3f,.6f,1.1f,1.7f,2.1f };
    static Float_t ey[5] = { .4f,.7f,1.2f,1.6f,1.9f };
    static Float_t ez[5] = { .1f,.8f,1.3f,1.5f,2.2f };
    static Float_t x[11] = { -.5f,-.3f,.1f,.6f,.9f,1.1f,1.3f,1.7f,1.9f,2.1f,2.5f 	    };
    static Float_t y[11] = { .1f,.4f,.6f,.7f,1.1f,1.2f,1.5f,1.6f,1.7f,1.9f,2.1f }	    ;
    static Float_t z__[11] = { -.2f,.1f,.5f,.8f,.9f,1.3f,1.4f,1.5f,2.1f,2.2f,	    2.5f };

    /* Format strings */
    static char fmt_804[] = "(/\002 ????? TEST OF FINT ... NARG =\002,i6,"
	    "\002 FINT =\002,e20.10,\002 ERROR CONDITION \002,a6,\002 NOT DET"
	    "ECTED.\002)";

    /* System generated locals */
    Int_t i__1, i__2, i__3;
    Float_t r__1, r__2, r__3;

    /* Builtin functions */
    Int_t s_wsfe(cilist *), do_fio(Int_t *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static Int_t i__, j, k;
    static Float_t t;
    static Int_t ij;
    static Float_t fx[2];
    static Int_t kx, lx[2], ly[2], lz[2], nx, ny, nz, ky, kz, nx2, ny2, nz2;
    static Float_t arg[3];
    static Int_t ijk, iex, iey, iez;
    static Bool_t okt;
    static Float_t fxy[4]	/* was [2][2] */;
    extern /* Subroutine */ int e104p(Int_t *, Float_t *, Int_t *, Float_t *, 
	    Float_t *, Float_t *, Bool_t *);
    extern Double_t 
      e104t1(Int_t *, Float_t *, Float_t *, Float_t *), 
      e104t2(Int_t *, Int_t *, Float_t *, Float_t *, Float_t *, Float_t *, Float_t *), 
      e104t3(Int_t *, Int_t *, Int_t *, Float_t *, Float_t *, Float_t *, 
	      Float_t *, Float_t *, Float_t *, Float_t *);
    static Int_t narg, iiey, iiez;
    static Float_t tref;
    extern Double_t fint(Int_t *, Float_t *, Int_t *, Float_t *, Float_t *);
    static Int_t nent[3];
    static Float_t fxyz[8]	/* was [2][2][2] */;
    static Int_t lxyz[55]	/* was [5][11] */;

    /* Fortran I/O blocks */
    static cilist io___39 = { 0, 6, 0, fmt_804, 0 };
    static cilist io___40 = { 0, 6, 0, fmt_804, 0 };
    --ent;
    --table;

    /* Function Body */


    *ok = kTRUE;
    nent[0] = 5;
    nent[1] = 5;
    for (i__ = 1; i__ <= 5; ++i__) {
	for (j = 1; j <= 3; ++j) {
/* L10: */
	    lxyz[i__ + j * 5 - 6] = 1;
	}
    }
    lxyz[16] = 1;
    lxyz[21] = 1;
    for (i__ = 3; i__ <= 5; ++i__) {
	for (j = 4; j <= 5; ++j) {
/* L15: */
	    lxyz[i__ + j * 5 - 6] = 2;
	}
    }
    lxyz[27] = 2;
    lxyz[32] = 2;
    for (j = 6; j <= 9; ++j) {
/* L20: */
	lxyz[j * 5 - 2] = 3;
    }
    lxyz[29] = 3;
    lxyz[34] = 3;
    for (j = 8; j <= 11; ++j) {
/* L25: */
	lxyz[j * 5 - 1] = 4;
    }
    for (k = 1; k <= 5; ++k) {
	for (j = 1; j <= 5; ++j) {
	    for (i__ = 1; i__ <= 5; ++i__) {
		ijk = i__ + nent[0] * (j - 1 + nent[1] * (k - 1));
/* L30: */
/* Computing 2nd power */
		r__1 = ex[i__ - 1];
/* Computing 3rd power */
		r__2 = ey[j - 1];
/* Computing 4th power */
		r__3 = ez[k - 1], r__3 *= r__3;
		table[ijk] = (r__1 * r__1 + r__2 * (r__2 * r__2) + r__3 * 
			r__3 + 10.f) * .1f;
	    }
	}
    }
    for (iex = 1; iex <= 5; ++iex) {
	nent[0] = iex;
	i__1 = iex;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L50: */
	    ent[i__] = ex[i__ - 1];
	}
	nx = (iex << 1) + 1;
	nx2 = nx - 2;
	for (iey = 1; iey <= 5; ++iey) {
	    nent[1] = iey;
	    i__1 = iey;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		iiey = i__ + iex;
/* L60: */
		ent[iiey] = ey[i__ - 1];
	    }
	    ny = (iey << 1) + 1;
	    ny2 = ny - 2;
	    for (iez = 1; iez <= 5; ++iez) {
		nent[2] = iez;
		i__1 = iez;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    iiez = iex + iey + i__;
/* L70: */
		    ent[iiez] = ez[i__ - 1];
		}
		nz = (iez << 1) + 1;
		nz2 = nz - 2;
		i__1 = nx;
		for (kx = 1; kx <= i__1; ++kx) {
		    narg = 1;
		    arg[narg - 1] = x[kx - 1];
		    lx[0] = lxyz[iex + kx * 5 - 6];
		    lx[1] = lx[0] + 1;
		    for (i__ = 1; i__ <= 2; ++i__) {
			fx[i__ - 1] = table[lx[i__ - 1]];
/* L110: */
		    }
		    tref = e104t1(&nx2, arg, &ex[lx[0] - 1], fx);
		    e104p(&narg, arg, nent, &ent[1], &table[1], &tref, &okt);
		    *ok = *ok && okt;
		    i__2 = ny;
		    for (ky = 1; ky <= i__2; ++ky) {
			narg = 2;
			arg[narg - 1] = y[ky - 1];
			ly[0] = lxyz[iey + ky * 5 - 6];
			ly[1] = ly[0] + 1;
			for (i__ = 1; i__ <= 2; ++i__) {
			    for (j = 1; j <= 2; ++j) {
				ij = lx[i__ - 1] + nent[0] * (ly[j - 1] - 1);
				fxy[i__ + (j << 1) - 3] = table[ij];
/* L120: */
			    }
			}
			tref = e104t2(&nx2, &ny2, arg, &arg[1], &ex[lx[0] - 
				1], &ey[ly[0] - 1], fxy);
			e104p(&narg, arg, nent, &ent[1], &table[1], &tref, &
				okt);
			*ok = *ok && okt;
			i__3 = nz;
			for (kz = 1; kz <= i__3; ++kz) {
			    narg = 3;
			    arg[narg - 1] = z__[kz - 1];
			    lz[0] = lxyz[iez + kz * 5 - 6];
			    lz[1] = lz[0] + 1;
			    for (i__ = 1; i__ <= 2; ++i__) {
				for (j = 1; j <= 2; ++j) {
				    for (k = 1; k <= 2; ++k) {
					ijk = lx[i__ - 1] + nent[0] * (ly[j - 
						1] - 1 + nent[1] * (lz[k - 1] 
						- 1));
					fxyz[i__ + (j + (k << 1) << 1) - 7] = 
						table[ijk];
/* L130: */
				    }
				}
			    }
			    tref = e104t3(&nx2, &ny2, &nz2, arg, &arg[1], &
				    arg[2], &ex[lx[0] - 1], &ey[ly[0] - 1], &
				    ez[lz[0] - 1], fxyz);
			    e104p(&narg, arg, nent, &ent[1], &table[1], &
				    tref, &okt);
			    *ok = *ok && okt;
/* L200: */
			}
/* L300: */
		    }
/* L400: */
		}
/* L500: */
	    }
/* L600: */
	}
/* L700: */
    }
/*          IF(      ERPRNT .AND.       ERSTOP) WRITE(*,801) */
/*          IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,802) */
/*          IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,803) */
    narg = 0;
    t = fint(&narg, arg, nent, &ent[1], &table[1]);
    if (t != 0.f) {
	*ok = kFALSE;
	s_wsfe(&io___39);
	do_fio(&c__1, (char *)&narg, (ftnlen)sizeof(Int_t));
	do_fio(&c__1, (char *)&t, (ftnlen)sizeof(Float_t));
	do_fio(&c__1, "E104.1", (ftnlen)6);
	e_wsfe();
    }
    narg = 6;
    t = fint(&narg, arg, nent, &ent[1], &table[1]);
    if (t != 0.f) {
	*ok = kFALSE;
	s_wsfe(&io___40);
	do_fio(&c__1, (char *)&narg, (ftnlen)sizeof(Int_t));
	do_fio(&c__1, (char *)&t, (ftnlen)sizeof(Float_t));
	do_fio(&c__1, "E104.1", (ftnlen)6);
	e_wsfe();
    }
    return ok;
/* L801: */
/* L802: */
/* L803: */
} /* e104ch_ */

/* Subroutine */ int e104p(Int_t *narg, Float_t *arg, Int_t *nent, Float_t *
	ent, Float_t *table, Float_t *tref, Bool_t *ok)
{
    /* Initialized data */

    static Int_t margin = 100;

    /* Format strings */
    static char fmt_10[] = "(/\002 *** ARITHMETIC ERROR ***\002,2i8,1x,e12.3"
	    ",3x,5e12.3)";

    /* System generated locals */
    Int_t i__1;
    Float_t r__1;

    /* Builtin functions */
    Int_t i_nint(Float_t *), s_wsfe(cilist *), do_fio(Int_t *, char *, 
	    ftnlen), e_wsfe(void);

    /* Local variables */
    static Int_t i__;
    static Float_t res;
    static Int_t irel;
    extern Double_t fint(Int_t *, Float_t *, Int_t *, Float_t *, Float_t *);
    static Float_t test, relpr;

    /* Fortran I/O blocks */
    static cilist io___46 = { 0, 6, 0, fmt_10, 0 };


/* #include "kernnumt/sysdat.inc" */
    /* Parameter adjustments */
    --nent;
    --arg;
    --ent;
    --table;

    /* Function Body */
    test = fint(narg, &arg[1], &nent[1], &ent[1], &table[1]);
    res = (r__1 = *tref - test, dabs(r__1));
    r__1 = res / relpr;
    irel = i_nint(&r__1);
    *ok = irel <= margin;
    if (! (*ok)) {
	s_wsfe(&io___46);
	do_fio(&c__1, (char *)&(*narg), (ftnlen)sizeof(Int_t));
	do_fio(&c__1, (char *)&irel, (ftnlen)sizeof(Int_t));
	do_fio(&c__1, (char *)&res, (ftnlen)sizeof(Float_t));
	i__1 = *narg;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&arg[i__], (ftnlen)sizeof(Float_t));
	}
	e_wsfe();
    }
    return 0;
} /* e104p_ */

Double_t e104t1(Int_t *nx, Float_t *x, Float_t *ax, Float_t *fx)
{
    /* System generated locals */
    Float_t ret_val;

    /* Local variables */
    static Float_t xi;

    /* Parameter adjustments */
    --fx;
    --ax;

    /* Function Body */
    ret_val = fx[1];
    if (*nx == 1) {
	return ret_val;
    }
    xi = (ax[2] - *x) / (ax[2] - ax[1]);
    ret_val = xi * fx[1] + (1.f - xi) * fx[2];
    return ret_val;
} /* e104t1_ */

Double_t e104t2(Int_t *nx, Int_t *ny, Float_t *x, Float_t *y, Float_t *ax, Float_t 
	*ay, Float_t *fxy)
{
    /* System generated locals */
    Float_t ret_val;

    /* Local variables */
    static Float_t fa[2], fx[2];
    extern Double_t e104t1(Int_t *, Float_t *, Float_t *, Float_t *);

    /* Parameter adjustments */
    fxy -= 3;
    --ay;
    --ax;

    /* Function Body */
    if (*ny == 1) {
	goto L10;
    }
    fx[0] = fxy[3];
    fx[1] = fxy[4];
    fa[0] = e104t1(nx, x, &ax[1], fx);
    fx[0] = fxy[5];
    fx[1] = fxy[6];
    fa[1] = e104t1(nx, x, &ax[1], fx);
    ret_val = e104t1(ny, y, &ay[1], fa);
    return ret_val;
L10:
    fx[0] = fxy[3];
    fx[1] = fxy[4];
    ret_val = e104t1(nx, x, &ax[1], fx);
    return ret_val;
} /* e104t2_ */

Double_t e104t3(Int_t *nx, Int_t *ny, Int_t *nz, Float_t *x, Float_t *y, 
	Float_t *z__, Float_t *ax, Float_t *ay, Float_t *az, Float_t *fxyz)
{
    /* System generated locals */
    Float_t ret_val;

    /* Local variables */
    static Int_t i__, j;
    static Float_t fz[2], fxy[4]	/* was [2][2] */;
    extern Double_t e104t1(Int_t *, Float_t *, Float_t *, Float_t *), e104t2(
	    Int_t *, Int_t *, Float_t *, Float_t *, Float_t *, Float_t *, Float_t *);

    /* Parameter adjustments */
    fxyz -= 7;
    --az;
    --ay;
    --ax;

    /* Function Body */
    if (*nz == 1) {
	goto L30;
    }
    for (i__ = 1; i__ <= 2; ++i__) {
	for (j = 1; j <= 2; ++j) {
/* L10: */
	    fxy[i__ + (j << 1) - 3] = fxyz[i__ + (j + 2 << 1)];
	}
    }
    fz[0] = e104t2(nx, ny, x, y, &ax[1], &ay[1], fxy);
    for (i__ = 1; i__ <= 2; ++i__) {
	for (j = 1; j <= 2; ++j) {
/* L20: */
	    fxy[i__ + (j << 1) - 3] = fxyz[i__ + (j + 4 << 1)];
	}
    }
    fz[1] = e104t2(nx, ny, x, y, &ax[1], &ay[1], fxy);
    ret_val = e104t1(nz, z__, &az[1], fz);
    return ret_val;
L30:
    for (i__ = 1; i__ <= 2; ++i__) {
	for (j = 1; j <= 2; ++j) {
/* L40: */
	    fxy[i__ + (j << 1) - 3] = fxyz[i__ + (j + 2 << 1)];
	}
    }
    ret_val = e104t2(nx, ny, x, y, &ax[1], &ay[1], fxy);
    return ret_val;
} 

void test() {
  enum (lw = 27000);
  Float_t w[lw];
  Int_t na = 15;
  Int_t nt = 125;
  Int_t lx = 0;
  Int_t ly = lx + na;
  if (e104ch(&na,&w[lx],&nt,&w[ly])) cout << "e104ch passed" << endl;
  else                               cout << "e104ch failed" << endl;
}
