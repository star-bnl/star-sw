/***************************************************************
 * $Id: StRichGHit.h,v 1.3 2000/02/08 16:23:44 lasiuk Exp $
 *
 * Description:
 *   StRichGHit is a data type containing information
 *   passed by Geant.
 *   
 *   StRichGHit is a structure/class with:
 *     - a 3-vector position (doubles)
 *     - a string with the particleID
 *     - a representation of the volumeID
 *     - the energy loss (double)
 *     - a constructor with only a long initialization list
 *       
 ***************************************************************
 * $Log: StRichGHit.h,v $
 * Revision 1.3  2000/02/08 16:23:44  lasiuk
 * change to class.  Augment constructors.
 * Incorporate system of units
 *
 * keep the track pointer info
 *
 * Revision 1.3  2000/02/08 16:23:44  lasiuk
 * change to class.  Augment constructors.
 * Incorporate system of units
 *
 * Revision 1.2  2000/01/27 17:05:37  lasiuk
 * add global information
 *
 * Revision 1.1  2000/01/18 21:32:01  lasiuk
 * Initial Revision
 *
 *   revision history:
 *     - 7/23/1999 created the struct,   Alexandre Nevski.
 *     - 7/27/1999 constructor added,    Alexandre Nevski.
 *     - 7/30/1999 fill added,           Alexandre Nevski.
 *     - 8/5/1999  problem with string,solved Alexandre Nevski. 
 ***************************************************************/
#ifndef ST_RICH_GHIT_H
#define ST_RICH_GHIT_H

#include <iostream.h>
#include <string>

#include "StThreeVector.hh"

#ifndef ST_NO_NAMESPACES
using std::string;
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
    StRichGHit(double x, double y, double z, int q, short pID);
class StRichGHit {
public:
    StRichGHit();

    StRichGHit(double x, double y, double z, double dE, short pID, string vID);
    StRichGHit(double x, double y, double z, int track_p, short pID);
    StRichGHit(double x, double y, double z, short pID);
    StRichGHit(double x, double y, double z, double dE, double ds, short pID, string vID,
	       double px, double py, double pz);
    ~StRichGHit();

    //StRichGHit(const StRichGHit&); { /* use default */}
    //StRichGHit& operator=(const StRichGHit&) {/* use default */}

    int     quadrant() const;
    const StThreeVector<double>& position()   const;
    StThreeVector<double>& position();
    int     trackp() const;
    const StThreeVector<double>& momentum() const;

    int     trackp()   const;
    double  cosX()     const;
    double  cosY()     const;
    void fill(double x, double y, double z, int q,
    double  dE()       const;
    short   id()       const;
    double  mass()     const;
    const string& volumeID() const;
    
    void fill(double x, double y, double z, int q,
	      double cosX, double coxY, double cosZ,
	      double step, double dE,
//     void fill(double,double,double,int,   -- momentum
// 	      double,double,double,double,
// 	      double,short,string);
	      short pID, string vID);

    void fill(double x, double y, double z, int track_p,
	      double cosX, double coxY, double cosZ, double step,
	      double dE, short pID, string vID);

    void addGlobal(double,double,double);

    void full(ostream&) const;
    int                      mQuad;
private:
    StThreeVector<double>    mXGlobal; //xx,yy,zz  NEVER USED!!!
    StThreeVector<double>    mXLocal;  //x,y,z
    StThreeVector<double>    mP;
    double                   mCosX, mCosY, mCosZ;
    int                      mTrackp;
    double                   mdS;
    string                   mVolumeId;  //!
    double                   mdE;
    double                   mMass;
};    
inline int     StRichGHit::quadrant() const {return mQuad;}

inline const StThreeVector<double>& StRichGHit::position() const {return mXLocal;}
inline StThreeVector<double>& StRichGHit::position() {return mXLocal;}
inline const StThreeVector<double>& StRichGHit::xGlobal()  const {return mXGlobal;}
inline const StThreeVector<double>& StRichGHit::momentum() const {return mP;}
inline int     StRichGHit::trackp() const {return mTrackp;}
inline double  StRichGHit::cosX()     const {return mCosX;}
inline double  StRichGHit::cosY()     const {return mCosY;}
inline double  StRichGHit::cosZ()     const {return mCosZ;}
inline double  StRichGHit::dE()       const {return mdE;}
inline short   StRichGHit::id()       const {return mId ;}
inline double  StRichGHit::mass()     const {return mMass;}
inline const string&  StRichGHit::volumeID() const {return mVolumeId ;}

#ifndef ST_NO_NAMESPACES
//} // namespace
#endif


#endif // StRichGHit_H
