/***************************************************************************
 *
 * $Id: StMixerMaker.h,v 1.11 2008/06/20 14:57:43 fisyak Exp $
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
 * Revision 1.11  2008/06/20 14:57:43  fisyak
 * Change interal presentation for ADC from UChat_t to Short_t
 *
 * Revision 1.10  2005/09/09 22:08:11  perev
 * IdTruth added
 *
 * Revision 1.9  2004/04/01 02:37:50  jeromel
 * Last commit forgot to advertize that writeFile() was removed but is used
 * in embedding. Corrected.
 * SetSequenceMerging added (matches TPCReader and tpcdaq_Maker methods). Before
 * that FCF mebedding would NOT run properly.
 *
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
//#include <utility>

// Dbs
class StTpcGeometry;
class StTpcSlowControl;
class StTpcElectronics;

// Processes 
class StMixerFastDigitalSignalGenerator;
class StTrsDigitalSignalGenerator;

// Containers
class StTrsAnalogSignal;
class StTrsSector;
#include "StTrsMaker/include/StTrsDigitalSector.hh"

// Output Data
#include "StTrsMaker/include/StTrsRawDataEvent.hh"

class StTrsOstream;
class StSequence;
class StTPCReader;
class StTrsDetectorReader;
class StTrsZeroSuppressedReader;

class StMixerReader {
public:
 StMixerReader();
 void Set(StTPCReader         *r);
 void Set(StTrsDetectorReader *r);
 int  getSequences(int sector,int row,int pad,int *nseq,StSequence **listOfSequences, UShort_t ***listIdTruth=0);//!
 int  getPadList  (int sector,int row, unsigned char **padList);
 StTPCReader 		   *TpcReader()         const {return mTpcReader;}
 StTrsDetectorReader       *TrsDetectorReader() const {return mTrsDetectorReader;}
 void Clear();
private:
 void SetSector(int sector);
private:
 int mSector;
 StTPCReader 		   *mTpcReader;
 StTrsZeroSuppressedReader *mTrsReader;
 StTrsDetectorReader       *mTrsDetectorReader;
};


class StMixerMaker : public StMaker {

 private:
    char            *gConfig[2]; //!
    StMixerMaker(const StMixerMaker&);
    StMixerMaker& operator=(const StMixerMaker&);

    // DataBases
    StTpcGeometry               *mGeometryDb; //!
    StTpcSlowControl            *mSlowControlDb; //!
    StTpcElectronics            *mElectronicsDb; //!
    
    // Processes
    StTrsDigitalSignalGenerator     *mDigitalSignalGenerator; //!

    // Container
    StTrsSector               *mSector; //!
    // I/O streams
    char*                        mOutputFileName; //!
    int                          mNumberOfEvents; //!

    // Output
    StTrsRawDataEvent            *mAllTheDataMixer; //!
   
    // to pass to tpcdaq_Maker
    char                         mMergeSequences;

    int                          mFirstSector;
    int                          mLastSector;

    StMixerReader                mReader[2];

 protected:

 public:
    StMixerMaker(const char *name="Mixer",char *kind1="undefined",char *kind2="undefined");
    ~StMixerMaker();
    Int_t  InitRun(int);
    Int_t  Make();
    Int_t  Finish();
    
    void Clear(Option_t *option="");  // called after every event to cleanup 

    char   *GetConfig(int i) {return gConfig[i-1];}

    int   writeFile(char*, int);   //!
    char SetSequenceMerging(char); //!

    virtual const char *GetCVS() const
      {
	static const char cvs[]="Tag $Name:  $ $Id: StMixerMaker.h,v 1.11 2008/06/20 14:57:43 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    
    ClassDef(StMixerMaker, 0)  // 
};

#endif
