/*************************************************************ASG.h**\
 * $Id: StRichAnalogSignalGenerator.h,v 1.1 2000/01/18 21:32:00 lasiuk Exp $
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
 *********************************************************************
 * $Log: StRichAnalogSignalGenerator.h,v $
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
 *
 *
 * Revision 1.2  2000/01/25 22:02:19  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
 *
********************************************************************/

 *     - 7/22/1999 created the class, Alexandre Nevski.
 *     - 8/18/1999 initial implementation, Caroline Peter.
 *     - 8/23/1999 noise added, C & A
 ***************************************************************************/
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
#include <functional>

#ifndef ST_NO_NAMESPACES

using std::pair;
#endif

#endif

    class StRichAnalogSignalGenerator : public binary_function<StRichGHit,double,void> {
    public:
	void operator()( const StRichGHit& , double ) const;
  
    private:
	double induceTension( double, double) const; 
    };
    double          mPadWidth;

    double          mAnodePadPlaneSpacing;
};

#ifndef ST_NO_NAMESPACES
//} 
#endif

#endif // ASG_H
