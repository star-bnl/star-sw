/*!
 * \class StEtrHit 
 * \author Ming Shao, Jan 2012
 */
/***************************************************************************
 *
 * $Id: StEtrHit.h,v 2.1 2012/01/24 03:06:12 perev Exp $
 *
 * Author: Ming Shao, Jan 2012
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEtrHit.h,v $
 * Revision 2.1  2012/01/24 03:06:12  perev
 * Add Etr
 *
 *
 * Revision 1.0  2012/01/05 Ming
 * Initial Version
 *
 **************************************************************************/
#ifndef StEtrHit_hh
#define StEtrHit_hh

#include "StHit.h"
#include "StMemoryPool.hh"

class StEtrPoint;

class StEtrHit : public StHit {
public:
    StEtrHit();
    StEtrHit(const StThreeVectorF&  position,
             int sector, int layer, float charge);
    ~StEtrHit();
    StDetectorId detector() const;

    int sector() const;        // 0-11
    int layer() const;         // 0-2
protected:
    ClassDef(StEtrHit,1)
};

inline int StEtrHit::sector() const		{return hardwarePosition()/10;}
inline int StEtrHit::layer()  const		{return hardwarePosition()%10;}
inline StDetectorId StEtrHit::detector() const 	{return kEtrId;}

#endif
