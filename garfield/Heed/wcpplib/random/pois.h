#ifndef POIS_H
#define POIS_H

namespace Heed {

/*
class PoisState
{public:
  double SECOND_RAN;
  int s_inited_SECOND_RAN;
  PoisState(void): SECOND_RAN(0.0), s_inited_SECOND_RAN(0) {}
};

extern PoisState pois_state;
*/
// Attention!!! In order to set state, it is not enough to set
// it for SRANLUX.
// It needs also to set/get pois_state.

long pois(const double amu, int &ierror);
//long pois (float AMU,int &IERROR);

// Now it calls SRANLUX
// This is translation of the program from CERNLIB with the following comment

//C
//C    POISSON GENERATOR
//C    CODED FROM LOS ALAMOS REPORT      LA-5061-MS
//C    PROB(N)=EXP(-AMU)*AMU**N/FACT(N)
//C        WHERE FACT(N) STANDS FOR FACTORIAL OF N
//C    ON RETURN IERROR.EQ.0 NORMALLY
//C              IERROR.EQ.1 IF AMU.LE.0.
//C

}

#endif
