/***************************************************************************
 *
 * $Id: StTrsWireBinEntry.hh,v 1.1 1998/11/10 17:12:13 fisyak Exp $
 *
 * Author: brian, May 1998 
 ***************************************************************************
 *
 * Description: Results from transport of a single StTrsMiniChargeSegment
 *
 ***************************************************************************
 *
 * $Log: StTrsWireBinEntry.hh,v $
 * Revision 1.1  1998/11/10 17:12:13  fisyak
 * Put Brian trs versin into StRoot
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

#ifdef __sun
#include <stdcomp.h>  // bool type
#endif

#include "StThreeVector.hh"
#include "StGlobalCoordinate.hh"

class StTrsWireBinEntry {
public:
    StTrsWireBinEntry(StThreeVector<double>, float);
    ~StTrsWireBinEntry();
    //StTrsWireBinEntry(const StTrsWireBinEntry&);
    //StTrsWireBinEntry& operator=(cont StTrsWireBinEntry&);

    // access functions
    StThreeVector<double> position()              const;
    float                 numberOfElectrons()     const;

    StThreeVector<double> position()                   ;
    void                  setNumberOfElectrons(float)  ;
    void                  scaleNumberOfElectrons(float);

#ifdef ST_NO_TEMPLATE_DEF_ARGS
    StTrsWireBinEntry();  // needed for allocator
#endif
    
private:
    StThreeVector<double> mPosition;
    float                 mNumberOfElectrons;
};
StThreeVector<double>
inline StTrsWireBinEntry::position() const {return mPosition;}
float inline StTrsWireBinEntry::numberOfElectrons()  const {return mNumberOfElectrons;}
StThreeVector<double>
inline StTrsWireBinEntry::position() {return mPosition;}
void inline StTrsWireBinEntry::setNumberOfElectrons(float num)  { mNumberOfElectrons = num;}

//Non Member Function
ostream& operator<<(ostream& os, const StTrsWireBinEntry& entry);
#endif
