/********************************************************************
 * $Id: StRichSelectWire.h,v 1.3 2000/02/08 16:31:56 lasiuk Exp $
 *
 * Description:
 *   StRichSelectWire is a function object containing the algorithm that
 *   indicates on which wire will the electron deposit.
 *    
 *   StRichSelectWire is used like a normal function, i.e.
 *   StRichSelectWire(const ionSeg&);
 *
 *   StRichSelectWire computes the position of the
 *   closest wire to a given hit. StRichSelectWire depends
 *   only on the detector geometry. 
 *
 *   revision history:
 *     - 7/22/1999 created the class, Alexandre Nevski.
 *     - 8/5/1999 initial implementation, Caroline Peter. 
 *
 ********************************************************************
 * $Log: StRichSelectWire.h,v $
 * Revision 1.3  2000/02/08 16:31:56  lasiuk
 * use dbs
 *
 * Revision 1.3  2000/02/08 16:31:56  lasiuk
 * use dbs
 *
 * Revision 1.2  2000/01/25 22:02:22  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:04  lasiuk
 * Initial Revision
 ********************************************************************/
#ifndef ST_RICH_SELECTWIRE_H
#define ST_RICH_SELECTWIRE_H

#include <functional>

#ifndef ST_NO_NAMESPACES
using std::unary_function;
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
#include "StRichRrsMacros.h"
#include "StRichGHit.h"

typedef double wirePosition;

struct StRichSelectWire : public unary_function<StRichGHit,wirePosition> {
    wirePosition operator()(const StRichGHit&) const;
};
    
#ifndef ST_NO_NAMESPACES
//} 
#endif

#endif
