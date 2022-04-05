/***************************************************************************
 *
 * $Id: StSimpleMagneticField.hh,v 1.7 2012/06/11 15:04:55 fisyak Exp $
 *
 * Author: Thomas Ullrich, May 1998 
 ***************************************************************************
 *
 * Description: Class is implemented as a Singleton
 *
 ***************************************************************************
 *
 * $Log: StSimpleMagneticField.hh,v $
 * Revision 1.7  2012/06/11 15:04:55  fisyak
 * std namespace
 *
 * Revision 1.6  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2000/01/10 23:11:29  lasiuk
 * Include MACROS for compatibility with SUN CC5.0
 *
 * Revision 1.4  1999/06/16 14:26:51  fisyak
 * Add flags for egcs on Solaris
 *
 * Revision 1.3  1999/03/15 13:45:59  lasiuk
 * include SystemOfUnits from SCL
 *
 * Revision 1.2  1999/01/20 16:38:56  lasiuk
 * add threevector capability
 *
 * Revision 1.1  1998/11/10 17:12:05  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/05/20 19:00:00  ullrich
 * Renamed getInstance() to instance().
 *
 * Revision 1.1  1998/05/20 14:58:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_SIMPLE_MAGNETIC_FIELD
#define ST_SIMPLE_MAGNETIC_FIELD

#include <Stiostream.h>

#if  defined(__sun) && ! defined(__GNUG__)
#include <stdcomp.h>
#endif
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "StMagneticField.hh"

class StSimpleMagneticField : public StMagneticField {
public:
    ~StSimpleMagneticField();
    
    static StMagneticField* instance();
    static StMagneticField* instance(const StThreeVector<double>& B);
    static StMagneticField* instance(const char* file);
    
    const StThreeVector<double>& at(const StGlobalCoordinate& gc) const;
    const StThreeVector<double>& at(const StThreeVector<double>& v) const;

private:
    StSimpleMagneticField();
    StSimpleMagneticField(const StThreeVector<double>&);
    StSimpleMagneticField(const char*);                 
    
private:
    StThreeVector<double> mB;
    static StMagneticField* mInstance;
};


//
//   Inline member functions
//

inline StMagneticField*
StSimpleMagneticField::instance()
{
    if (mInstance == 0) {
	std::cerr << "StSimpleMagneticField::getInstance(): " << endl;	
	std::cerr << "\tWARNING" << endl;
	std::cerr << "\tNo arguments for instantiation of" << endl;
	std::cerr << "\tsingleton class. Zero magnetic field." << endl;
        mInstance = new StSimpleMagneticField;
    }
    return mInstance;	
}

inline const StThreeVector<double>&
StSimpleMagneticField::at(const StGlobalCoordinate&) const {return mB;}

inline const StThreeVector<double>&
StSimpleMagneticField::at(const StThreeVector<double>&) const {return mB;}
#endif
