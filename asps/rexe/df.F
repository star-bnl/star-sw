#ifdef  __GNUC__
#define C_RAD_PER_DEG  0.017453292519943295769237
#define C_DEG_PER_RAD 57.295779513082320876798155
      double precision function dsind(degree)
      double precision degree
      dsind = sin (C_RAD_PER_DEG*degree)
      end

      double precision function dcosd(degree)
      double precision degree
      dcosd = cos (C_RAD_PER_DEG*degree)
      end
      double precision function dtand(degree)
      double precision degree
      dtand = tan (C_RAD_PER_DEG*degree)
      end
      double precision function dasind(arg)
      double precision arg
      dasind = C_DEG_PER_RAD*asin (arg)
      end
      double precision function dacosd(arg)
      double precision arg
      dacosd = C_DEG_PER_RAD*acos (arg)
      end
      double precision function datand(arg)
      double precision arg
      datand = C_DEG_PER_RAD*atan (arg)
      end
      double precision function datan2d(arg1,arg2)
      double precision arg1,arg2
      datan2d = C_DEG_PER_RAD*atan2 (arg1,arg2)
      end
      real function ran(dummy)
      real dummy, rndm
      ran = rndm(dummy)
      end
#undef C_RAD_PER_DEG
#undef C_DEG_PER_RAD
#endif
