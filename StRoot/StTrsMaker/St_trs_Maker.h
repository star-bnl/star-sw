// $Id: St_trs_Maker.h,v 1.2 1999/01/22 11:43:09 lasiuk Exp $
// $Log: St_trs_Maker.h,v $
// Revision 1.2  1999/01/22 11:43:09  lasiuk
// add output structures
//
// Revision 1.1  1998/11/10 17:11:57  fisyak
// Put Brian trs versin into StRoot
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:31  perev
// cleanup
//
// Revision 1.5  1998/08/26 12:15:13  fisyak
// Remove asu & dsl libraries
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
#ifndef STAR_St_trs_Maker
#define STAR_St_trs_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_trs_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

// Dbs
class StTpcGeometry;
class StTpcSlowControl;
class StMagneticField;
class StTpcElectronics;
class StTrsDeDx;

// Processes 
class StTrsChargeTransporter;
class StTrsAnalogSignalGenerator;
class StTrsDigitalSignalGenerator;

// Containers
class StTrsWireHistogram;
class StTrsSector;
class StTrsDigitalSector;

// Output Data
//class StTpcRawDataEvent;
class St_trs_Maker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: St_trs_Maker.h,v 1.2 1999/01/22 11:43:09 lasiuk Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
// St_stk_stkpar *m_stk_stkpar;  //! pointer to stk parameters

#ifndef ST_NO_TEMPLATE_DEF_ARGS
   //vector<StTrsChargeSegment>  *mData; //
#else
   //vector<StTrsChargeSegment,allocator<StTrsChargeSegment> >  *mData; //
#endif

    // DataBases
    StTpcGeometry               *mGeometryDb; //!
    StTpcSlowControl            *mSlowControlDb; //!
    StMagneticField             *mMagneticFieldDb;//!
    StTpcElectronics            *mElectronicsDb; //!
    StTrsDeDx                   *mGasDb;//!
    
    // Processes 
    StTrsChargeTransporter      *mChargeTransporter;//!
    StTrsAnalogSignalGenerator  *mAnalogSignalGenerator;//!
    StTrsDigitalSignalGenerator *mDigitalSignalGenerator;//!
    
    // Container
    StTrsWireHistogram           *mWireHistogram;//!
    StTrsSector                  *mSector;//!
    StTrsDigitalSector           *mDigitalSector;//!

    //StTpcRawDataEvent          *mAllTheData;
protected:
public: 
                  St_trs_Maker(const char *name="tpc_raw", const char *title="event/raw_data/tpc");
   virtual       ~St_trs_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   ClassDef(St_trs_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
