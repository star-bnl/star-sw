/**
 * @file StiEmcIsActiveFunctor.h
 * @class StiEmcIsActiveFunctor
 * @brief function object for determine a EMC padrow's active regions
 *
 * @author Claude A  Pruneau, Wayne State University
 * @date Oct 2002
 */

#ifndef StiEmcIsActiveFunctor_H_INCLUDED
#define StiEmcIsActiveFunctor_H_INCLUDED

#include "Sti/StiIsActiveFunctor.h"

//class StDetectorDbEmcRDOMasks;

class StiEmcIsActiveFunctor : public StiIsActiveFunctor
{
  public:
  StiEmcIsActiveFunctor(int iSector, int iLayer);
  virtual ~StiEmcIsActiveFunctor();
  virtual bool operator()(double dYlocal, double dZlocal) const;
    
protected:
    /// returns the RDO board number [1-6] for the tpc padrow [1-45]
    inline static int rdo(int iSector, int iLayer);

    /// pointer to instance of RDO mask
    //    static StDetectorDbEmcRDOMasks *_rdoMasks;

    /// is the east half on?
    bool _eastActive;
    /// is the west half on?
    bool _westActive;
};

int StiEmcIsActiveFunctor::rdo(int iSector, int iLayer)
{
  int iRdo = 0;
  return iRdo;
} 

#endif 
