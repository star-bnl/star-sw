/**
 * @file StiTpcIsActiveFunctor.h
 * @class StiTpcIsActiveFunctor
 * @brief function object for determine a TPC padrow's active regions
 *
 * @author Ben Norman, Kent State University
 * @date March 2002
 */

#ifndef STI_TPC_IS_ACTIVE_FUNCTOR
#define STI_TPC_IS_ACTIVE_FUNCTOR

#include "Sti/StiIsActiveFunctor.h"

class StDetectorDbTpcRDOMasks;

class StiTpcIsActiveFunctor : public StiIsActiveFunctor{
  public:
    /// construct an IsActiveFunctor representing one TPC padrow 
    /// spanning both TPC halves.  The sector should be [1-12],
    /// based on the half in the west TPC.  Padrow is in [1-45].
    StiTpcIsActiveFunctor(int iSector, int iPadrow);
    virtual ~StiTpcIsActiveFunctor();
    virtual bool operator()(double dYlocal, double dZlocal);
    
protected:
    /// returns the RDO board number [1-6] for the tpc padrow [1-45]
    inline static int rdoForPadrow(int iPadrow);

    /// pointer to instance of RDO mask
    static StDetectorDbTpcRDOMasks *s_pRdoMasks;

    /// is the east half of the padrow on?
    bool m_bEastActive;
    /// is the west half of the padrow on?
    bool m_bWestActive;
};

///Function returns the rdo board number for a given 
///padrow index. 
///Range of map used is 1-45. 
int StiTpcIsActiveFunctor::rdoForPadrow(int iPadrow){
  ++iPadrow;
  int iRdo = 0;
  if (iPadrow>0&&iPadrow<=8){
    iRdo = 1;
  }
  else if (iPadrow>8&&iPadrow<=13){
    iRdo = 2;
  }
  else if (iPadrow>13&&iPadrow<=21){
    iRdo = 3;
  }
  else if (iPadrow>21&&iPadrow<=29){
    iRdo = 4;
  }
  else if (iPadrow>29&&iPadrow<=37){
    iRdo = 5;
  }
  else if (iPadrow>37&&iPadrow<=45){
    iRdo = 6;
  }
  return iRdo;
} // rdoForPadrow

#endif // ifndef STI_TPC_IS_ACTIVE_FUNCTOR
