/***************************************************************************
 *
 * $Id: StTrsWireBinEntry.hh,v 1.9 2004/04/07 18:56:26 perev Exp $
 *
 * Author: brian, May 1998 
 ***************************************************************************
 *
 * Description: Results from transport of a single StTrsMiniChargeSegment
 *
 ***************************************************************************
 *
 * $Log: StTrsWireBinEntry.hh,v $
 * Revision 1.9  2004/04/07 18:56:26  perev
 * Improve alignment
 *
 * Revision 1.8  2003/12/24 13:44:52  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.7  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.6  2000/07/30 02:42:10  long
 * add d()
 *
 * Revision 1.5  2000/06/23 00:12:24  snelling
 * Removed dependence on local files now pointed to StDbUtilities
 *
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
#include <Stiostream.h>

#if  defined(__sun) && ! defined(__GNUG__)
#include <stdcomp.h>  // bool type
#endif

#include "StThreeVector.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"

class StTrsWireBinEntry {
public:
    StTrsWireBinEntry(StThreeVector<double>&, float,double,double,double *, int);
    ~StTrsWireBinEntry();
    //StTrsWireBinEntry(const StTrsWireBinEntry&);
    //StTrsWireBinEntry& operator=(cont StTrsWireBinEntry&);

    // access functions
    const StThreeVector<double>& position()       const;
    float                 numberOfElectrons()     const;
    double                sigmaL();
    double                sigmaT();
     double *              d();
    
    StThreeVector<double>& position()                  ;
    int                   id() const {return mId;} 
    void                  setNumberOfElectrons(float)  ;
    void                  scaleNumberOfElectrons(float);

#ifdef ST_NO_TEMPLATE_DEF_ARGS
    StTrsWireBinEntry();  // needed for allocator
#endif
    
private:
    StThreeVector<double> mPosition;
    double                mSigmaL;
    double                mSigmaT;  
    double                mD[3];
    float                 mNumberOfElectrons;
    int                   mId;
};
inline const StThreeVector<double>&
StTrsWireBinEntry::position() const {return mPosition;}
inline float StTrsWireBinEntry::numberOfElectrons()  const {return mNumberOfElectrons;}
inline StThreeVector<double>& StTrsWireBinEntry::position() {return mPosition;}
void inline StTrsWireBinEntry::setNumberOfElectrons(float num)  { mNumberOfElectrons = num;}

//Non Member Function
ostream& operator<<(ostream& os, const StTrsWireBinEntry& entry);
#endif
