//==================================================================================================
// StFtpcMixerMaker: Merges simulated and real data (embedding), needs two input chains
//
// Author: Frank Simon (fsimon@bnl.gov)
//==================================================================================================


#ifndef STAR_StFtpcMixerMaker
#define STAR_StFtpcMixerMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#ifndef HEP_SYSTEM_OF_UNITS_H
#include "SystemOfUnits.h"
#endif

#include <utility>

class StSequence;
class StDAQReader;
class StFTPCReader;
class StFtpcDbReader;

// Db classes
class St_ftpcDimensions;
class St_ftpcPadrowZ;
class St_ftpcEField;
class St_ftpcVDrift;
class St_ftpcDeflection;
class St_ftpcdVDriftdP;
class St_ftpcdDeflectiondP;
class St_ftpcAmpSlope;
class St_ftpcAmpOffset;
class St_ftpcTimeOffset;
class St_ftpcDriftField;
class St_ftpcGas;
class St_ftpcElectronics;
class St_ftpcAsicMap;


class StFtpcMixerMaker : public StMaker {

 private:
    const Char_t            *mConfig1; //!
    const Char_t            *mConfig2; //!
    StFtpcMixerMaker(const StFtpcMixerMaker&);
    StFtpcMixerMaker& operator=(const StFtpcMixerMaker&);

    // DataBases
    St_ftpcDimensions    *m_dimensions;            //!
    St_ftpcPadrowZ       *m_padrow_z;              //!
    St_ftpcAsicMap       *m_asicmap;               //!
    St_ftpcEField        *m_efield;                //!
    St_ftpcVDrift        *m_vdrift;                //!
    St_ftpcDeflection    *m_deflection;            //!
    St_ftpcdVDriftdP     *m_dvdriftdp;             //!
    St_ftpcdDeflectiondP *m_ddeflectiondp;         //!
    St_ftpcAmpSlope      *m_ampslope;              //!
    St_ftpcAmpOffset     *m_ampoffset;             //!
    St_ftpcTimeOffset    *m_timeoffset;            //!
    St_ftpcDriftField    *m_driftfield;            //!
    St_ftpcGas           *m_gas;                   //!
    St_ftpcElectronics   *m_electronics;           //!

    // Processes

    // Container

    // I/O streams
    int                          mNumberOfEvents; //!

    StDAQReader *daqr1;
    StFTPCReader *ftpcr1;
    StDAQReader *daqr2;
    StFTPCReader *ftpcr2;

    // Database
    StFtpcDbReader *dbReader;


    // Output

 protected:

 public:
    StFtpcMixerMaker(const char *name="FtpcMixer", const char *kind1="undefined", const char *kind2="undefined");
    ~StFtpcMixerMaker();
    Int_t  InitRun(int);
    Int_t  Make();
    Int_t  Finish();
    
    void Clear(Option_t *option="");  // called after every event to cleanup 

    inline const Char_t   *getConfig1() {return mConfig1;}
    inline const Char_t   *getConfig2() {return mConfig2;}

    virtual const char *GetCVS() const
      {static const char cvs[]="Tag $Name:  $ $Id: StFtpcMixerMaker.h,v 1.3 2014/08/06 11:43:17 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
 
    ClassDef(StFtpcMixerMaker,0) 
};

#endif

 /***************************************************************************
 *
 * $Id: StFtpcMixerMaker.h,v 1.3 2014/08/06 11:43:17 jeromel Exp $
 *
 * $Log: StFtpcMixerMaker.h,v $
 * Revision 1.3  2014/08/06 11:43:17  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.2  2003/09/10 19:47:17  perev
 * ansi corrs
 *
 * Revision 1.1  2003/02/14 18:11:25  fsimon
 * Initial commit of FTPC embedding code
 *
 *
 ***************************************************************************/
