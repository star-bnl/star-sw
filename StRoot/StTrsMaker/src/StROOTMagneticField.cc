/***************************************************************************
 *
 * $Id: StROOTMagneticField.cc,v 1.1 1999/03/23 03:39:21 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Class is implemented as a Singleton
 *
 ***************************************************************************
 *
 * $Log: StROOTMagneticField.cc,v $
 * Revision 1.1  1999/03/23 03:39:21  lasiuk
 * Initial Revsion
 *
 **************************************************************************/
#ifdef __ROOT__

#include "StROOTMagneticField.hh"

StMagneticField* StROOTMagneticField::mInstance = 0;	// static member

StROOTMagneticField::StROOTMagneticField() {/* noop */}

StROOTMagneticField::StROOTMagneticField(const StThreeVector<double>& v)
    : mB(v) {/* noop */}

StROOTMagneticField::~StROOTMagneticField() {/* noop */}

StMagneticField*
StROOTMagneticField::instance()
{
    StThreeVector<double> B(0,0,.5);
    if (mInstance == 0) 
	mInstance = new StROOTMagneticField(B);
    else {
	cerr << "StROOTMagneticField::getInstance(): " << endl;	
	cerr << "\tWARNING" << endl;
	cerr << "\tSingleton class is already instantiated." << endl;
	cerr << "\tArgument (StThreeVector) is ignored." << endl;
    }
    return mInstance;
}

StMagneticField*
StROOTMagneticField::instance(const StThreeVector<double>& B)
{
    if (mInstance == 0)
	mInstance = new StROOTMagneticField(B);
    else {
	cerr << "StROOTMagneticField::getInstance(): " << endl;	
	cerr << "\tWARNING" << endl;
	cerr << "\tSingleton class is already instantiated." << endl;
	cerr << "\tArgument (StThreeVector) is ignored." << endl;
    }
    return mInstance;
}
#endif
