/******************************************************
 * $Id: StRichASCIIReader.cxx,v 1.5 2000/06/07 01:42:23 lasiuk Exp $
 *
 * Description:
 *******************************************************
 * $Log: StRichASCIIReader.cxx,v $
 * Revision 1.5  2000/06/07 01:42:23  lasiuk
 * exit/abort ultimatum
 *
 * Revision 1.4  2000/02/11 13:09:56  lasiuk
 * new version ASCII reader (include track_p)
 *
 * Revision 1.3  2000/02/08 16:20:20  lasiuk
 * for standalone __ROOT__ defn'd.
 * switch order of arguments, remove quadrant algebra
 *
 * Revision 1.2  2000/01/28 20:35:46  lasiuk
 * inline removed from .cxx files
 *
 * Revision 1.1  2000/01/27 17:06:00  lasiuk
 * Initial Revision
 *
 ******************************************************/
#ifndef __ROOT__
#include "StGlobals.hh"
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
    
#include "StRichASCIIReader.h"
#include "StRichGHit.h"

StRichASCIIReader::StRichASCIIReader()
{
    cout << "StRichASCIIReader::StRichASCIIReader()" << endl;
    abort();
}

StRichASCIIReader::StRichASCIIReader(string& file)
    : mIfs(file.c_str())
{
    if(!mIfs || mIfs.bad()) {
	cout << "StRichASCIIReader::StRichASCIIReader(string&)" << endl;
	cout << "Cannot Open file " << file.c_str() << endl;
	abort();
    }
}

int StRichASCIIReader::operator()(StRichGHit& hit )
{
    if ( read() == 0 ) {
	hit.fill( mX.x(), mX.y(), mX.z(), mQuadrant,
		  (mP.x()/abs(mP)), (mP.y()/abs(mP)), (mP.z()/abs(mP)),
		  mdS, mdE, mTrackP , mGV );
	hit.addGlobal(mXX.x(), mXX.y(), mXX.z());
	return 0;
    }
    else {
	return 1;
    }
}
 

int StRichASCIIReader::whichQuadrant(StThreeVector<double>& x)
{
    double box = x.x();
    return 1;
}

int StRichASCIIReader::whichVolume(int val)
{
    //
    // coding from GEANT is:
    //    volume+Isys*1000
    // where:
    //  Isys = 1 for RGAP
    //  Isys = 2 for RCSI
    int volume = val/1000;
    switch(volume) {
    case 1:
        mGV = string("RGAP");
        break;
    case 2:
        mGV = string("RCSI");
        break;
    default:
        mGV = string("");
        cerr << "StRchMaker::whicVolume() UNKNOWN Volume" << endl;
        break;
    }
    int volumeNumber = (val - (volume*1000));
    return volumeNumber;
}

int StRichASCIIReader::read() 
{
    float x[3];
    float xx[3];
    float p[3];
    mIfs >> x[0] >> x[1] >> x[2] >> xx[0] >> xx[1];
    mIfs >> xx[2] >> p[0] >> p[1] >> p[2] >> mTof;
    mIfs >> mdE >> mdS >> mVolume >> mTrackP >> mNumber;
    mX = StThreeVector<double>(x[0],x[1],x[2]);
    mXX = StThreeVector<double>(xx[0],xx[1],xx[2]);
    mP = StThreeVector<double>(p[0],p[1],p[2]);
     PR(mX);
     PR(mP);
     PR(mTof);
     PR(mdE);
     PR(mdS);
     PR(mVolume);
     PR(mTrackP);
     PR(mNumber);
    mQuadrant = whichVolume(mVolume);
    return 0;
}
    
#ifndef ST_NO_NAMESPACES
//}
#endif
#endif
