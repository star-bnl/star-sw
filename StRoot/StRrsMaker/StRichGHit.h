/************************************************************GHit.h**\
 * $Id: StRichGHit.h,v 1.2 2000/01/27 17:05:37 lasiuk Exp $
 *
 * Description:
 *   StRichGHit is a data type containing information passed by Geant.
 *   
 *   StRichGHit is a simple CStruct with:
 *     - a 3-vector position (doubles)
 *     - a string with the particleID
 *     - a representation of the volumeID
 *     - the energy loss (double)
 *     - a constructor with only a long initialization list
 *       
 *
 *   StRichGHit is part of the StRichRawData namespace - belongs to
 *   RawData Project.
 * *****************************************************************
 * $Log: StRichGHit.h,v $
 * Revision 1.2  2000/01/27 17:05:37  lasiuk
 * add global information
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
 *     - 8/5/1999  problem with string...solved, Alexandre Nevski. 
 *
 ********************************************************************/

 *   revision history:
 *     - 7/23/1999 created the struct,   Alexandre Nevski.
 *     - 7/27/1999 constructor added,    Alexandre Nevski.
 *     - 7/30/1999 fill added,           Alexandre Nevski.
 *     - 8/5/1999  problem with string,solved Alexandre Nevski. 
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
#include <iostream.h>
#include <string>

#include "StThreeVector.hh"

#ifndef ST_NO_NAMESPACES
using std::string;
struct StRichGHit {
	
    StRichGHit(double x_, double y_, double z_,double dE_, short pID_, string vID_)
	: x(x_), y(y_), z(z_), id(pID_), mVolumeID(vID_), dE(dE_) { }
	
    StRichGHit(double x_, double y_, double z_, int q_, short pID_) 
	: x(x_), y(y_), z(z_), quad(q_), id(pID_) { }

    StRichGHit(double x_, double y_, double z_,double dE_, short pID_, string vID_,double xx_, double yy_, double zz_)
	: x(x_), y(y_), z(z_), id(pID_), mVolumeID(vID_), dE(dE_),xx(xx_),yy(yy_),zz(zz_) { }
    StRichGHit() { }
	
    void fill(double,double,double,int,
	      double,double,double,double,
	      double,short,string);
    double  cosY()     const;
	      short pID, string vID);
    
    double                   xx,yy,zz;
    double                   x,y,z;
    int                      quad;
    double                   cosX, cosY, cosZ;
    double                   step;
    short                    id;
    string                   mVolumeID;  //!
    double                   dE;
};
    
    ostream& operator<<(ostream&, const StRichGHit&);
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
