/************************************************************
 * $Id: StRichFilter.h,v 1.2 2000/01/25 22:02:19 lasiuk Exp $
 *
 * Description:
 *  Filter decides which algorithms to apply depending on the kind
 *  of received particle.
 *  In this implementation, Filter examines which particle will 
 *  induce a signal, such as the charged particle in CH4 (RGAP) or 
 *  the Cerenkov photon in cesium iodide (RCSI). The former produces
 *  a serie of ionizations, the latter kicks out a low energy 
 *  electron by photoeffect. The charged particle in RCSI is rejected.
 *  Note: Cerenkov photons have negative energy loss by 
 *  convention (GEANT), while charged particles do not.
 *
 *  Also, in this file, are implementated two coordinate transformation
 *  procedures, that convert all hits to RGAP local coordinates with
 *  associated quadrant numbers.
 *
 ***********************************************************************
 * $Log: StRichFilter.h,v $
 * Revision 1.2  2000/01/25 22:02:19  lasiuk
 * Second Revision
 *
 * Revision 1.3  2000/02/08 16:22:57  lasiuk
 * move selection into maker.  Remove from package next revision
 *
 * Revision 1.2  2000/01/25 22:02:19  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
 *
 *  revision history:
 *     - 8/9/1999 created the class,               Alexandre Nevski.

 *
 ************************************************************************/
#ifdef NEVER
#ifndef ST_RICH_FILTER_H
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500

#include <functional>
#ifndef ST_NO_NAMESPACES
using std::unary_function;
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#include "StRichGeometryDb.h"
#include "StRichRrsMacros.h"
    struct StRichFilter : public unary_function<StRichGHit,void> {
	void operator()(StRichGHit& );
	void changeCoord(StRichGHit& );
	void whatQuadrant(StRichGHit& );

    };
struct StRichFilter : public unary_function<StRichGHit,void> {
    void operator()(StRichGHit& );    
};

#ifndef ST_NO_NAMESPACES    
#endif

#endif
#endif
