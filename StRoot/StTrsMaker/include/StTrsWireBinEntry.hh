/***************************************************************************
 *
 * $Id: StTrsWireBinEntry.hh,v 1.4 2000/02/24 16:39:52 long Exp $
 *
 * Author: brian, May 1998 
 ***************************************************************************
 *
 * Description: Results from transport of a single StTrsMiniChargeSegment
 *
 ***************************************************************************
 *
 * $Log: StTrsWireBinEntry.hh,v $
 * Revision 1.4  2000/02/24 16:39:52  long
 * add sigmaL(),sigmaT()
 *
 * Revision 1.3  1999/06/16 14:26:51  fisyak
 * Add flags for egcs on Solaris
 *
 * Revision 1.2  1999/01/18 10:17:54  lasiuk
 * constructor by reference
 * set functions by reference
 *
 * Revision 1.1  1998/11/10 17:12:13  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/03 15:58:38  lasiuk
 * added set member functions
 * added scaleNumberOfElectrons()
 *
 * Revision 1.1  1998/06/04 23:32:21  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_WIRE_BIN_ENTRY_HH
#define ST_TRS_WIRE_BIN_ENTRY_HH
#include <iostream.h>

#if  defined(__sun) && ! defined(__GNUG__)
#include <stdcomp.h>  // bool type
#endif

#include "StThreeVector.hh"
#include "StGlobalCoordinate.hh"

class StTrsWireBinEntry {
public:
    StTrsWireBinEntry(StThreeVector<double>&, float,double,double);
    ~StTrsWireBinEntry();
    //StTrsWireBinEntry(const StTrsWireBinEntry&);
    //StTrsWireBinEntry& operator=(cont StTrsWireBinEntry&);

    // access functions
    const StThreeVector<double>& position()       const;
    float                 numberOfElectrons()     const;
    double                sigmaL();
    double                sigmaT();
    
    StThreeVector<double>& position()                  ;
    void                  setNumberOfElectrons(float)  ;
    void                  scaleNumberOfElectrons(float);

#ifdef ST_NO_TEMPLATE_DEF_ARGS
    StTrsWireBinEntry();  // needed for allocator
#endif
    
private:
    StThreeVector<double> mPosition;
    float                 mNumberOfElectrons;
    double                mSigmaL;
    double                mSigmaT;
};
inline const StThreeVector<double>&
StTrsWireBinEntry::position() const {return mPosition;}
inline float StTrsWireBinEntry::numberOfElectrons()  const {return mNumberOfElectrons;}
inline StThreeVector<double>& StTrsWireBinEntry::position() {return mPosition;}
void inline StTrsWireBinEntry::setNumberOfElectrons(float num)  { mNumberOfElectrons = num;}

//Non Member Function
ostream& operator<<(ostream& os, const StTrsWireBinEntry& entry);
#endif
