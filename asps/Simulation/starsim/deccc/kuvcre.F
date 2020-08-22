*
* $Id: kuvcre.F,v 1.1.1.1 1996/03/08 15:32:54 mclareni Exp $
*
* $Log: kuvcre.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
*#include "kuip/pilot.h"
#if defined(CERNLIB_UNIX)
# ifndef CERNLIB_QFMSOFT
#   define CERNLIB_BSLASH
# elif defined(CERNLIB_BSLASH)
#    undef CERNLIB_BSLASH
# endif
#endif

#if (defined(CERNLIB_UNIX))||(!defined(CERNLIB_SINGLE))

#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif

#endif

#if defined(CERNLIB_VAXVMS)

#ifndef CERNLIB_VAX
#define CERNLIB_VAX
#endif
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif

#endif

#if defined(CERNLIB_QMVAOS)
#ifndef CERNLIB_ALPHAOSF
#define CERNLIB_ALPHAOSF
#endif
#endif

#if defined(CERNLIB_QMALPH)
#ifndef CERNLIB_ALPHAVMS
#define CERNLIB_ALPHAVMS
#define CERNLIB_BSLASH
#endif
#endif

#if defined(CERNLIB_ALPHAVMS) || defined(CERNLIB_ALPHAOSF)
#ifndef CERNLIB_ALPHA
#define CERNLIB_ALPHA
#endif
#endif

#if defined(CERNLIB_ALPHAVMS) || defined(CERNLIB_VAXVMS)
#ifndef CERNLIB_VMS
#define CERNLIB_VMS
#endif
#endif


*CMZ :  2.03/05 17/08/93  15.24.33  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE KUVCRE(VNAME,TYPE,LENGTH,LLOW,LHIGH)
*
********************************************************************************
*
* Create vector VNAME of type TYPE ('R' for real or 'I' for integer) and length
* LENGTH, and returns LLOW and LHIGH address inside Q or IQ array;
* example the vector can be accessed by Q(LLOW:LHIGH) if TYPE='R'
* or IQ(LLOW:LHIGH) if TYPE='I'.
* Vector length array (dimensioned to 3) with LENGTH(I) containing
* the I-th dimension length (0 or 1 if the dimension is not used);
* example LENGTH(1)=10 and LENGTH(2)=LENGTH(3)=0 or 1 define a mono-dimensional
* vector of length 10
* If LLOW=LHIGH=0 an error occurred.
*
*     Input :
*
      CHARACTER*(*) VNAME
      CHARACTER*(*) TYPE
      INTEGER LENGTH(3)
*
*     Output :
*
      INTEGER LLOW
      INTEGER LHIGH
*
********************************************************************************
*
*#include "kuip/kcques.inc"
*
* $Id: kcques.inc,v 1.1.1.1 1996/03/08 15:32:59 mclareni Exp $
*
* $Log: kcques.inc,v $
* Revision 1.1.1.1  1996/03/08 15:32:59  mclareni
* Kuip
*
*
#ifndef CERNLIB_KUIP_KCQUES_INC
#define CERNLIB_KUIP_KCQUES_INC
*
*
* kcques.inc
*
      COMMON /QUEST/ IQUEST(100)


#endif
      CHARACTER*64 VECNAM
*
      IQUEST(1)=0
      LLOW=0
      LHIGH=0
      IF (LENGTH(2).EQ.0) LENGTH(2)=1
      IF (LENGTH(3).EQ.0) LENGTH(3)=1
      L=LENOCC(VNAME)
      WRITE (VECNAM,1000) VNAME(1:L),LENGTH(1),LENGTH(2),LENGTH(3)
1000  FORMAT (A,'(',I6,',',I6,',',I6,')')
      CALL KXCRV1(VECNAM,TYPE)
      IF (IQUEST(1).NE.0) GO TO 999
      CALL KUVECT(VNAME,LLOW,LHIGH)
999   END



