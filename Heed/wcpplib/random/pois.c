#include <math.h>

#include "wcpplib/random/pois.h"
#include "wcpplib/random/rnorm.h"
#include "wcpplib/random/ranluxint.h"
#include "wcpplib/stream/prstream.h"

/*
// to avoid one else file put it here
#ifdef PRINT_RANLUX
unsigned long num_ranlux = 0;
#endif 

PoisState pois_state;
*/
     
long pois (double AMU,int &IERROR)
//C
//C    POISSON GENERATOR
//C    CODED FROM LOS ALAMOS REPORT      LA-5061-MS
//C    PROB(N)=EXP(-AMU)*AMU**N/FACT(N)
//C        WHERE FACT(N) STANDS FOR FACTORIAL OF N
//C    ON RETURN IERROR.EQ.0 NORMALLY
//C              IERROR.EQ.1 IF AMU.LE.0.
//C
{
  double AMUOL = -1.;
  double AMAX = 100.;
  double EXPMA = 0.;
  double PIR = 0;
  double RAN;
  //static double SECOND_RAN;
  //static int s_inited_SECOND_RAN = 0;
  long N=0;
  IERROR=0;
  if(AMU>AMAX) goto m500;
  if(AMU==AMUOL) goto m200;
  if(AMU>0.0) goto m100  ;
//C    MEAN SHOULD BE POSITIVE
  IERROR=1;
  return 0;
//C    SAVE EXPONENTIAL FOR FURTHER IDENTICAL REQUESTS
 m100: IERROR=0;
  AMUOL=AMU;
  EXPMA=exp(-AMU);
m200: PIR=1.;
  N=-1;
m300: N=N+1;
  //{  // for debug
  //  double x = SRANLUX();
  //  mcout<<"pois: x = "<<x<<'\n';
  //  PIR=PIR * x;
  //}
  PIR = PIR * SRANLUX();  // working variant
  if (PIR > EXPMA) goto m300;
  return N;
//C   NORMAL APPROXIMATION FOR AMU.GT.AMAX
m500:       
  /*
  //Iprint2n(mcout, 
  //	   pois_state.s_inited_SECOND_RAN, 
  //	   pois_state.SECOND_RAN);
  if(pois_state.s_inited_SECOND_RAN == 1)
  {
    pois_state.s_inited_SECOND_RAN = 0;
    N = long(pois_state.SECOND_RAN*sqrt(AMU) + AMU + .5);
    return N;
  }
  else
  {
    rnorm_double(SRANLUX(), SRANLUX(), RAN, pois_state.SECOND_RAN);
    N = long(RAN*sqrt(AMU) + AMU + .5);
    pois_state.s_inited_SECOND_RAN = 1;
    return N;
  }
  */
  RAN = rnorm_improved();
  N = long(RAN*sqrt(AMU) + AMU + .5);
  return N;

}

/*     
long pois (float AMU,int &IERROR)
//C
//C    POISSON GENERATOR
//C    CODED FROM LOS ALAMOS REPORT      LA-5061-MS
//C    PROB(N)=EXP(-AMU)*AMU**N/FACT(N)
//C        WHERE FACT(N) STANDS FOR FACTORIAL OF N
//C    ON RETURN IERROR.EQ.0 NORMALLY
//C              IERROR.EQ.1 IF AMU.LE.0.
//C
{
	  float AMUOL=-1.;
	  float AMAX=100.;
	  float EXPMA;
	  float PIR=0;
		float RAN,DUMMY;
	  long N=0;
	  IERROR=0;
	  if(AMU>AMAX) goto m500;
	  if(AMU==AMUOL) goto m200;
	  if(AMU>0.) goto m100  ;
//C    MEAN SHOULD BE POSITIVE
	  IERROR=1;
	  return 0;
//C    SAVE EXPONENTIAL FOR FURTHER IDENTICAL REQUESTS
 m100: IERROR=0;
	  AMUOL=AMU;
	  EXPMA=exp(-AMU);
m200: PIR=1.;
	  N=-1;
m300: N=N+1;
	  //{  // for debug
	  //  double x = SRANLUX();
	  //  mcout<<"pois: x = "<<x<<'\n';
	  //  PIR=PIR * x;
	  //}
	  PIR=PIR * SRANLUX();  // working variant
	  if(PIR>EXPMA) goto m300;
	  return N;
//C   NORMAL APPROXIMATION FOR AMU.GT.AMAX
m500:       
	  rnorm(SRANLUX(), SRANLUX(), RAN,DUMMY);
	  N = long(RAN*sqrt(AMU) + AMU + .5);
	  return N;
}
*/
 
