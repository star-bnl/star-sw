/***************************************************************************
 *
 * $Id: StMixerMaker.h,v 1.8 2003/12/24 13:44:54 fisyak Exp $
 *
 * Author: Patricia Fachini
 *
 ***************************************************************************
 *
 * Description: Maker for doing the embedding (mixing)
 *              
 ***************************************************************************
 *
 * $Log: StMixerMaker.h,v $
 * Revision 1.8  2003/12/24 13:44:54  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.7  2002/03/12 22:36:48  pfachini
 * Changing Init() to InitRun(int)
 *
 * Revision 1.6  2000/08/11 14:56:17  pfachini
 * *** empty log message ***
 *
 * Revision 1.3  2000/03/15 17:26:09  pfachini
 * Using now the Trs classes instead of having a 'local' copy
 *
 * Revision 1.2  2000/02/22 20:25:34  pfachini
 * *** empty log message ***
 *
 * Revision 1.1  2000/02/16 21:02:10  pfachini
 * First version StMixer
 *
 *
 **************************************************************************/

#ifndef STAR_StMixerMaker
#define STAR_StMixerMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#ifndef HEP_SYSTEM_OF_UNITS_H
#include "SystemOfUnits.h"
#endif

//#include <vector>
#include <utility>

// Dbs
class StTpcGeometry;
class StTpcSlowControl;
class StTpcElectronics;

// Processes 
class StMixerFastDigitalSignalGenerator;
class StTrsDigitalSignalGenerator;
class StMixerEmbedding;

// Containers
class StTrsAnalogSignal;
class StTrsSector;
class StTrsDigitalSector;

// Output Data
class StTrsRawDataEvent;
#if 0
class StTrsOstream;
#endif
class StTpcRawDataEvent;
class StSequence;

class StMixerMaker : public StMaker {

 private:
    Char_t            *gConfig1; //!
    Char_t            *gConfig2; //!
    StMixerMaker(const StMixerMaker&);
    StMixerMaker& operator=(const StMixerMaker&);

    // DataBases
    StTpcGeometry               *mGeometryDb; //!
    StTpcSlowControl            *mSlowControlDb; //!
    StTpcElectronics            *mElectronicsDb; //!
    
    // Processes
    StTrsDigitalSignalGenerator     *mDigitalSignalGenerator; //!
    StMixerEmbedding                *mEmbedding; //!

    // Container
    StTrsSector               *mSector; //!
    StTrsSector               *mSector1; //!
    StTrsSector               *mSector2; //!
    StTrsDigitalSector        *mDigitalSector; //!
#if 0
    // I/O streams
    char*                        mOutputFileName; //!
    StTrsOstream*                mOutputStreamMixer; //!
    int                          mNumberOfEvents; //!
#endif
    // Output
    StTrsRawDataEvent            *mAllTheDataMixer; //!
   
    int                          mFirstSector;
    int                          mLastSector;

 protected:

 public:
    StMixerMaker(const char *name="Mixer",char *kind1="undefined",char *kind2="undefined");
    ~StMixerMaker();
    Int_t  InitRun(int);
    Int_t  Make();
    Int_t  Finish();
    
    void Clear(Option_t *option="");  // called after every event to cleanup 

    Char_t   *GetConfig1() {return gConfig1;}
    Char_t   *GetConfig2() {return gConfig2;}
    int   getSequences1(int sector,int row,int pad,int *nseq,StSequence **listOfSequences);//!
    int   getSequences2(int sector,int row,int pad,int *nseq,StSequence **listOfSequences);//!
#if 0
    int   writeFile(char*, int); //!
#endif
    virtual const char *GetCVS() const
      {
	static const char cvs[]="Tag $Name:  $ $Id: StMixerMaker.h,v 1.8 2003/12/24 13:44:54 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    
    ClassDef(StMixerMaker, 2)  // StAF chain virtual base class for Makers
};

#endif
