/***************************************************************************
 *
 * $Id: StMixerMaker.h,v 1.2 2000/02/22 20:25:34 pfachini Exp $
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
class StMixerDigitalSignalGenerator;
class StMixerEmbedding;

// Containers
class StMixerAnalogSignal;
class StMixerSector;
class StMixerDigitalSector;

// Output Data
class StMixerDataEvent;
class StMixerOstream;
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
    StMixerDigitalSignalGenerator   *mDigitalSignalGenerator; //!
    StMixerEmbedding                *mEmbedding; //!

    // Container
    StMixerSector               *mSector;//!
    StMixerSector               *mSector1;//!
    StMixerSector               *mSector2;//!
    StMixerDigitalSector        *mDigitalSector;//!

    // I/O streams
    char*                        mOutputFileName;//!
    StMixerOstream*              mOutputStreamMixer;//!
    int                          mNumberOfEvents;//!

    // Output
    StMixerDataEvent             *mAllTheDataMixer;//!
   
    int                          mFirstSector;
    int                          mLastSector;

 protected:

 public:
    StMixerMaker(const char *name="Mixer",char *kind1="undefined",char *kind2="undefined");
    ~StMixerMaker();
    Int_t  Init();
    Int_t  Make();
    Int_t  Finish();
    Int_t  Clear();
    
    Char_t   *GetConfig1() {return gConfig1;}
    Char_t   *GetConfig2() {return gConfig2;}
    int   getSequences1(int sector,int row,int pad,int *nseq,StSequence **listOfSequences);//!
    int   getSequences2(int sector,int row,int pad,int *nseq,StSequence **listOfSequences);//!
    int   writeFile(char*, int);//!

    virtual const char *GetCVS() const
      {
	static const char cvs[]="Tag $Name:  $ $Id: StMixerMaker.h,v 1.2 2000/02/22 20:25:34 pfachini Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    
    ClassDef(StMixerMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
