* $Id: math_constants.inc,v 1.3 1999/05/10 21:04:54 fisyak Exp $
* $Log: math_constants.inc,v $
* Revision 1.3  1999/05/10 21:04:54  fisyak
* remove double defintion of C_DEG_PER_RAD
*
* Revision 1.2  1999/04/13 21:08:33  sakrejda
* #define replaced by the PARAMETER statements
*
* Revision 1.1  1998/02/19 15:56:21  fisyak
* Math. Phys. constants from H.Ward
*
** Most of these were copied from math.h.  The AIX math.h was
** different than the other three, so I am simply going to have
** our own version.
**
** Underscores here often mean 'divided by'.  E.g., PI_2 means pi/2.

      DOUBLE PRECISION C_DEG_PER_RAD,C_RAD_PER_DEG,C_2PI,C_EULER,C_1_PI,C_2_PI,
     > C_2_SQRTPI, C_E, C_LN10, C_LN2, C_LOG10E, C_LOG2E, C_PI, C_PI_2,
     > C_PI_4, C_SQRT1_2, C_SQRT2


      PARAMETER(  C_RAD_PER_DEG=  0.017453292519943295769237)
      PARAMETER(  C_2PI=          6.28318530717958648)
      PARAMETER(  C_EULER=        0.577215664901532860606512)
      PARAMETER(  C_1_PI=         0.31830988618379067154)
      PARAMETER(  C_2_PI=         0.63661977236758134308)
      PARAMETER(  C_2_SQRTPI=     1.12837916709551257390)
      PARAMETER(  C_E=            2.7182818284590452354)
      PARAMETER(  C_LN10=         2.30258509299404568402)
      PARAMETER(  C_LN2=          0.69314718055994530942)
      PARAMETER(  C_LOG10E=       0.43429448190325182765)
      PARAMETER(  C_LOG2E=        1.4426950408889634074)
      PARAMETER(  C_PI=           3.14159265358979323846)
      PARAMETER(  C_PI_2=         1.57079632679489661923)
      PARAMETER(  C_PI_4=         0.78539816339744830962)
      PARAMETER(  C_SQRT1_2=      0.70710678118654752440)
      PARAMETER(  C_SQRT2=        1.41421356237309504880)






