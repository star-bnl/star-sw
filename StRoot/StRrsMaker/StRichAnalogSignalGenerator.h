/***************************************************************************
 * $Id: StRichAnalogSignalGenerator.h,v 1.5 2000/03/17 14:54:15 lasiuk Exp $
 *
 * Description:
 *   StRichAnalogSignalGenerator is a function object containing the 
 *   algorithm that simulates an analog electronic signal on a
 *   specific pad.
 *    
 *   StRichAnalogSignalGenerator is used like a normal function, 
 *   i.e. StRichAnalogSignalGenerator(geant_hit,amplification factor);
 *   
 *   StRichAnalogSignalGenerator generates signals on pads
 *   by calculating the nearest pad (row and col) and
 *   distributing a given charge on the area hit.
 *
 ***************************************************************************
 * $Log: StRichAnalogSignalGenerator.h,v $
 * Revision 1.5  2000/03/17 14:54:15  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.4  2000/03/12 23:56:33  lasiuk
 * new coordinate system
 * exchange MyRound with inline templated funtion
 *
 * Revision 1.3  2000/02/08 16:21:43  lasiuk
 * use coordinate transformation routines for pad limits
 * incorporation of dbs
 *
 * Revision 1.2  2000/01/25 22:02:19  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
 *
 *   revision history:
 *     - 7/22/1999 created the class, Alexandre Nevski.
 *     - 8/18/1999 initial implementation, Caroline Peter.
 *     - 8/23/1999 noise added, C & A
 ***************************************************************************/
#ifndef ST_RICH_ANALOG_SIGNAL_GENERATOR_H
#define ST_RICH_ANALOG_SIGNAL_GENERATOR_H

#include <utility>

#ifndef ST_NO_NAMESPACES
using std::pair;
#endif

#include "StRichRrsMacros.h"

#include "StRichWriter.h"
#include "StRichGHit.h"
#include "StRichMiniHit.h"
#include "StRichCoordinates.h"
class StRichGeometryDb;
class StRichCoordinateTransform;
class StRichWriter;

class StRichAnalogSignalGenerator {
public:
    static StRichAnalogSignalGenerator* getInstance(StRichWriter*);
    
    ~StRichAnalogSignalGenerator();

    //StRichAnalogSignalGenerator(const StRichAnalogSignalGenerator&) {/* use default */}
    //StRichAnalogSignalGenerator& operator=(const StRichAnalogSignalGenerator&) {/*use default*/}

    void induceSignal(const StRichMiniHit* , double );
    
    pair<int, int> calculatePadLimits(const StRichRawCoordinate&) const;
    pair<int, int> calculateRowLimits(const StRichRawCoordinate&) const;

protected:
    StRichAnalogSignalGenerator();
    StRichAnalogSignalGenerator(StRichWriter*);

private:
    double induceTension(double, double) const;

private:
    StRichGeometryDb*          mGeomDb;
    StRichCoordinateTransform* mTransform;
    StRichWriter*              mOutput;
    
    int             mNumberOfPadsInRowQ;
    int             mNumberOfRowsInColumnQ;

    double          mPadLength;
    double          mPadWidth;

    double          mAnodePadPlaneSpacing;

    static StRichAnalogSignalGenerator* mInstance;
};

#ifndef ST_NO_NAMESPACES
//} 
#endif

#endif // ASG_H
