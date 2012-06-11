/***************************************************************************
 *
 * $Id: StSimpleMagneticField.cc,v 1.5 2012/06/11 15:04:55 fisyak Exp $
 *
 * Author: Thomas Ullrich, May 1998 
 ***************************************************************************
 *
 * Description: Class is implemented as a Singleton
 *
 ***************************************************************************
 *
 * $Log: StSimpleMagneticField.cc,v $
 * Revision 1.5  2012/06/11 15:04:55  fisyak
 * std namespace
 *
 * Revision 1.4  1999/03/16 01:59:27  lasiuk
 * Use Units at Initialization
 *
 * Revision 1.3  1999/03/15 13:45:44  lasiuk
 * units consistency added here (tesla)
 *
 * Revision 1.2  1999/01/18 21:02:45  lasiuk
 * comment diagnostics
 *
 * Revision 1.1  1998/11/10 17:12:20  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/05/20 19:02:05  ullrich
 * Renamed getInstance() to instance().
 *
 * Revision 1.1  1998/05/20 14:59:28  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StSimpleMagneticField.hh"
#include "StGetConfigValue.hh"
using namespace std;
StMagneticField* StSimpleMagneticField::mInstance = 0;	// static member

StSimpleMagneticField::StSimpleMagneticField() {/* noop */}

StSimpleMagneticField::StSimpleMagneticField(const StThreeVector<double>& v)
    : mB(v) {/* noop */}

StSimpleMagneticField::StSimpleMagneticField(const char* filename)
{
    StGetConfigValue(filename, "StSimpleMagneticField.mB", mB);
    //PR(mB);
    // Make sure units are correct
    mB *= tesla;
    PR(mB/tesla);
}

StSimpleMagneticField::~StSimpleMagneticField() {/* noop */}

StMagneticField*
StSimpleMagneticField::instance(const StThreeVector<double>& B)
{
    if (mInstance == 0)
	mInstance = new StSimpleMagneticField(B);
    else {
	cerr << "StSimpleMagneticField::getInstance(): " << endl;	
	cerr << "\tWARNING" << endl;
	cerr << "\tSingleton class is already instantiated." << endl;
	cerr << "\tArgument (StThreeVector) is ignored." << endl;
    }
    return mInstance;
}

StMagneticField* StSimpleMagneticField::instance(const char* file)
{
    if (mInstance == 0)
	mInstance = new StSimpleMagneticField(file);
    else {
	cerr << "StSimpleMagneticField::getInstance(): " << endl;	
	cerr << "\tWARNING" << endl;
	cerr << "\tSingleton class is already instantiated." << endl;
	cerr << "\tArgument (const char*) is ignored." << endl;
    }
    return mInstance;
}



