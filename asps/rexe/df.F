#ifdef  __GNUC__
      double precision function dsind(degree)
      double precision degree
#include "math_constants.inc"
      dsind = sin (C_RAD_PER_DEG*degree)
      end
      double precision function dcosd(degree)
      double precision degree
#include "math_constants.inc"
      dcosd = cos (C_RAD_PER_DEG*degree)
      end
      double precision function dtand(degree)
      double precision degree
#include "math_constants.inc"
      dtand = tan (C_RAD_PER_DEG*degree)
      end
      double precision function dasind(arg)
      double precision arg
#include "math_constants.inc"
      dasind = C_DEG_PER_RAD*asin (arg)
      end
      double precision function dacosd(arg)
      double precision arg
#include "math_constants.inc"
      dacosd = C_DEG_PER_RAD*acos (arg)
      end
      double precision function datand(arg)
      double precision arg
#include "math_constants.inc"
      datand = C_DEG_PER_RAD*atan (arg)
      end
      double precision function datan2d(arg1,arg2)
      double precision arg1,arg2
#include "math_constants.inc"
      datan2d = C_DEG_PER_RAD*atan2 (arg1,arg2)
      end
      real function ran(dummy)
      real dummy, rndm
      ran = rndm(dummy)
      end
#endif
