// $Id: StEmcRawMaker.h,v 1.10 2014/08/06 11:43:07 jeromel Exp $

/*!\class StEmcRawMaker
\author Alexandre A. P. Suaide
 
This class copies the BEMC raw data from DAQ file into StEvent.
This make should run only in production or when reading DAQ
files. The tasks performed by this maker are:
 
  1. Get EMC data from DAQ files \n
  2. Fills B+E emcRawData with the daq data \n
  3.a Check for BEMC corruption but does not remove BEMC sub-event. \n
  3.b Check for EEMC corruption and  does remove EEMC sub-event. \n
  4. Fills StEmcRawHits depending on the BEMC settings defined
     by the user. For EEMC works only daqReader->StEvent. \n
  5. Fills some QA histograms for BEMC & EEMC \n

The controlADCtoE tables can be partially controlled in the
BFC chain by use of the GoptEMC option. The option is immediately
followed by 6 hexadecimal numbers corresponding to the 6 components
of the EMC system as follows:

  BTOW \n
  ETOW \n
  BSMD \n
  ESMD \n
  BPSD \n
  EPSD \n

...where PSD means both pre and post shower components.
Each hexadecimal number is a bit patter of what to do for that
particular detector. Implemented so far are values of 1 or 0 for
these bits in the BEMC (where bit 0 is the least significant bit):

 bit 0: sets CheckStatus to 1 or 0 \n
 bit 1: sets CutOffType to 1 or 0 \n

An example would be using the chain option "GoptEMC000020" which
would set CheckStatus to 0 for BTOW+BSMD+BPSD, and CutOffType to
0 for BTOW+BSMD and 1 for BPSD.
*/

#ifndef STAR_StEmcRawMaker
#define STAR_StEmcRawMaker

#include "StMaker.h"
#include "StRTSBaseMaker.h"
#include "TH1.h"
#include "TH2.h"
#include "tables/St_controlADCtoE_Table.h"
#include "StBemcRaw.h"
#include "defines.h"

class StEmcCollection;
class StEmcDecoder;
class StEmcGeom;
class StEmcRawData;
class StEvent;

class StEEmcDb;
class StEemcRaw;

class StEmcRawMaker : public StRTSBaseMaker
{
protected:
    StEvent*                 mEvent;
    StBemcRaw*               mBemcRaw;
    StEemcRaw*               mEemcRaw;
    StEEmcDb*                eeStDb; ///< to assess EEMC DB

    void                     fillHistograms();///<Fill QA histograms
    Bool_t                   prepareEnvironment();///< Prepare the StEvent environment to fill the EMC data
    Bool_t                   makeBemc(); ///< Make the Barrel-EMC detector
    Bool_t                   makeEemc(); ///< Make the Endcap-EMC detector

public:
    StEmcRawMaker(const char *name="EmcRaw"); ///< StEmcRawMaker constructor
    virtual                   ~StEmcRawMaker(); ///< StEmcRawMaker destructor
    virtual Int_t             Init(); ///< Init function. This method initializes the histograms
    virtual Int_t             InitRun(Int_t runumber); ///< InitRun function
    virtual Int_t             Make(); ///< Process each event
    virtual Int_t             Finish(); ///< Finish function.

    StRtsTable* Dta()
      {
	return DaqDta();
      }

    StRtsTable* GetDaqElement(const char *elementPath)
      {
	return GetNextDaqElement(elementPath);
      }


    StBemcRaw*                getBemcRaw()
    {
        return mBemcRaw;
    } ///< Return the StBemcRaw pointer
    void                      setPrint(Bool_t a); ///< Obsolete function; users can control messages with logger config file.

    virtual const char *      GetCVS() const
    {
        static const char cvs[]="Tag $Name:  $ $Id: StEmcRawMaker.h,v 1.10 2014/08/06 11:43:07 jeromel Exp $ built " __DATE__ " " __TIME__ ;
        return cvs;
    }

    ClassDef(StEmcRawMaker, 1)
};

#endif

// $Log: StEmcRawMaker.h,v $
// Revision 1.10  2014/08/06 11:43:07  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.9  2009/02/04 21:05:42  kocolosk
// Refactor StEEmcDb(Maker), new location for StEmcDecoder. Fixes RT #1388.
//
// Revision 1.8  2009/01/27 19:58:36  mattheww
// Updates to StEmcRawMaker to be compatible with 2009 DAQ Format
//
// Revision 1.7  2008/03/27 19:54:16  genevb
// Utilize new BFC option for GoptEMC for controlADCtoE table
//
// Revision 1.6  2007/01/22 19:13:37  kocolosk
// use STAR logger for all output
//
// Revision 1.5  2006/01/16 11:12:00  suaide
// tower map bug fixed and astyle run
//
// Revision 1.4  2004/10/21 00:01:50  suaide
// small changes in histogramming and messages for BEMC
// Complete version for EEMC done by Jan Balewski
//
// Revision 1.3  2004/10/19 23:48:49  suaide
// Initial implementation of the endcap detector done by Jan Balewski
//
// Revision 1.2  2004/10/19 17:53:00  suaide
// code clean up
//
// Revision 1.1  2004/10/18 18:20:07  suaide
// New Maker. Will replace StEmcADCtoEMaker in production.
// It reads only DAQ structures. Output is StEvent.
//
//
