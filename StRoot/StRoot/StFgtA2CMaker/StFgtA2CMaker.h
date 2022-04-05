/*!
 * \class StFgtA2CMaker 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtA2CMaker.h,v 1.24 2014/08/06 11:43:09 jeromel Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: Converts the ADC value to charge and optionally
 * removes strips with status not passing the mask.  Computing the
 * charge currently involves
 *
 * 1) pedestal subtraction
 * 2) applying minimum threshold (both fixed and multiples of the pedistal st. err.)
 * 3) fitting the pulse shape
 * 3) applying gain
 *
 * The status map is applied as follows: status 0x00 is good, all else
 * is bad.  Strips are removed if the status bit anded with the mask
 * is non-zero. To remove all strips with any status bit set
 * set the mask to 0xFF.  To ignore status, set the mask to 0x00.  To remove only
 * strips with bit 3 set, set the mask to 0x04, etc.  Status is
 * currently only a uchar.
 *
 * Strips with no "signal" (as defined by the thresholds) are marked
 * as no signal, and the strip charge is marked as invalid.
 *
 * Strips are only removed from StEvent if the status bit bitwise
 * anded with the mask is non-zero or if the pedestal value is outside
 * the possible dynamic range (0-kFgtMaxAdc).
 * 
 ***************************************************************************
 *
 * $Log: StFgtA2CMaker.h,v $
 * Revision 1.24  2014/08/06 11:43:09  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.23  2013/03/14 01:45:43  akio
 * fix some kStFgtNumTimebins -> dynamic local mMaxTimeBin from StFgtCollection
 * Seed Type 3 & 4 changed, and 5 goone
 * 3 = 3 timbins in row above thr, tbin0<peak/3, last tbin<peak
 * 4 = 3 timbins in row above thr, tbin0<peak/3
 *
 * Revision 1.22  2012/11/27 17:32:51  akio
 * Adding option to read ped & status from text file. Default is reading from DB.
 *
 * Revision 1.21  2012/11/08 18:28:15  akio
 * - Split seedTypes3 into raising (kFgtSeedTypes3) and falling (kFgtSeedTypes4)
 * - Adding new seed Type (kFgtSeedTypes5) with 3 timebins in row above 3 sigma, and not raising nor falling
 *      You can disable this by setLeastRestrictiveSeed(false)
 * - Charge uncertainty factor can be adjusted by setPedSigFactor4Charge() [default is 1.0]
 *
 * Revision 1.20  2012/07/31 20:08:11  jeromel
 * Changes to make maker compatible with running in chain (was not)
 *
 * Revision 1.19  2012/07/06 01:12:17  avossen
 * implemented scaled pulse finder
 *
 * Revision 1.18  2012/07/05 21:39:47  avossen
 * added flag to allow long pulses
 *
 * Revision 1.17  2012/04/13 18:56:56  sgliske
 * More adjustments based on the review:
 * - Lastest StEvents from Thomas U.
 * - StFgtA2CMaker can no longer remove strips other than bad status or bad ped
 * - other related updates
 *
 * Revision 1.16  2012/03/07 17:46:55  sgliske
 * Added options for not removing strips
 *
 * Revision 1.15  2012/03/07 17:09:05  sgliske
 * code removed from compiling by #ifdef completely removed
 *
 * Revision 1.14  2012/03/06 21:21:32  sgliske
 * Responces to reviewers incoorperated.
 * White space and comments cleaned up.
 * Few remaining items offset with #ifdef,
 * which may get removed before final move to DEV
 *
 * Revision 1.13  2012/02/28 19:32:25  avossen
 * many changes to enable new clustering algo: New strip fields, identification of seed strips, passing neighboring strips, new order in strip collections
 *
 * Revision 1.12  2012/01/31 08:26:53  sgliske
 * cleaned up, and removed need to use setFgtDb.
 * Now, if not set, will try to find it using
 * GetMakerInheritsFrom
 *
 * Revision 1.11  2012/01/30 21:49:33  avossen
 * removed references to files
 *
 * Revision 1.10  2012/01/30 11:40:04  sgliske
 * a2cMaker now fits the pulse shape,
 * strip containers updated
 *
 * Revision 1.9  2012/01/30 10:42:22  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.8  2012/01/28 11:22:53  sgliske
 * changed status check to status map
 * changed setDb to setFgtDb
 * cleaned up few other minor things
 *
 * Revision 1.7  2012/01/24 06:52:46  sgliske
 * made status cuts optional
 * and updated status to a fail condition--
 * i.e. status == 0x0 is good, otherwise is bad.
 * WARNING--this may be different than that used at first
 * in for the cosmic test stand.
 *
 * Revision 1.6  2012/01/24 05:54:51  sgliske
 * changed default name to reflect A2C,
 * as opposed to old CorMaker
 *
 * Revision 1.5  2012/01/06 17:48:00  sgliske
 * Added requested GetCVS tag
 *
 * Revision 1.4  2011/12/01 00:13:23  avossen
 * included use of db. Note: For DB use it hast to be set with
 * setDb. Instantiate StFgtDBMaker, get the StFgtDb from the getTables
 * method and give the pointer to the A2C maker
 *
 * Revision 1.3  2011/11/25 20:24:13  ckriley
 * added statusmaker functionality
 *
 * Revision 1.2  2011/11/01 18:46:14  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.1  2011/10/28 14:58:49  sgliske
 * replacement to StFgtCorAdcMaker
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_A2C_MAKER_H_
#define _ST_FGT_A2C_MAKER_H_

#include <string>
#include "StMaker.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StEvent/StEnumerations.h"

class StFgtDb;

class StFgtA2CMaker : public StMaker {
 public:
   // constructors
   StFgtA2CMaker( const Char_t* name = "fgtA2CMaker" );

   // default OK
   // StFgtA2CMaker(const StFgtA2CMaker&);

   // deconstructor
   ~StFgtA2CMaker();

   // equals operator -- default OK
   // StFgtA2CMaker& operator=(const StFgtA2CMaker&);

   Int_t Init();
   Int_t InitRun(Int_t runumber);
   Int_t Make();

   // modifiers
   void setAbsThres( Float_t thres );     /// set to below -kFgtMaxAdc (-4096) to skip cut
   void setRelThres( Float_t thres );     /// set to zero to skip cut
   void setFgtDb( StFgtDb *fgtDb);        /// set pointer to StFgtDb
   void doCutBadStatus();                 /// set status mask to 0xFF, so any bad status is cut
   void setStatusMask( UChar_t mask );    /// set status mask to some other value
   void acceptLongPulses(Bool_t accept );
   void setClusterThreshold( Float_t threshold );
   void setPedSigFactor4Charge( Float_t factor);
   void useLeastRestrictiveSeed(Bool_t v);

   // reading pedestal from text file. filename="" (default) forces to read from DB
   void setPedestalFile(const Char_t* filename);
   void setStatusFile(const Char_t* filename);

   // cvs tag
   virtual const char *GetCVS() const
   {static const char cvs[]="Tag $Name:  $ $Id: StFgtA2CMaker.h,v 1.24 2014/08/06 11:43:09 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

 protected:
   Short_t checkValidPulse(StFgtStrip* pStrip, Float_t ped);
   // parameters
   Bool_t mAcceptLongPulses;
   UChar_t mStatusMask;
   Float_t mAbsThres, mRelThres,mClusterThreshold,mPedSigFactor4Charge;
   Bool_t mUseLeastRestrictiveSeed;

   // for reading pedestal from a text file
   std::string mPedFilename;
   Float_t mPed[kFgtNumElecIds],mPedRMS[kFgtNumElecIds];
   Bool_t mReadPedFile;
   std::string mStatusFilename;
   Int_t mStatus[kFgtNumElecIds];
   Bool_t mReadStatusFile;

   // pointer to the DB
   StFgtDb* mDb;

 private:   
   void readPedFile(Int_t elecId, Float_t &ped, Float_t &pedrms);
   void readStatusFile(Int_t elecId, UInt_t &status);
   ClassDef(StFgtA2CMaker,1);
   int mMaxTimeBin;
}; 

// inline functions

// deconstructor
// inline StFgtA2CMaker::~StFgtA2CMaker(){ /* */ };

// modifiers
inline void StFgtA2CMaker::acceptLongPulses( Bool_t accept ){        mAcceptLongPulses = accept; };
inline void StFgtA2CMaker::setClusterThreshold( Float_t threshold ){ mClusterThreshold = threshold; };
inline void StFgtA2CMaker::setAbsThres( Float_t thres ){             mAbsThres = thres; };
inline void StFgtA2CMaker::setRelThres( Float_t thres ){             mRelThres = thres; };
inline void StFgtA2CMaker::setFgtDb(StFgtDb* db ){                   mDb=db; };
inline void StFgtA2CMaker::doCutBadStatus(){                         mStatusMask = 0xFF; };
inline void StFgtA2CMaker::setStatusMask( UChar_t mask ){            mStatusMask = mask; };
inline void StFgtA2CMaker::setPedSigFactor4Charge( Float_t factor){  mPedSigFactor4Charge=factor;}
inline void StFgtA2CMaker::useLeastRestrictiveSeed(Bool_t v){        mUseLeastRestrictiveSeed=v;}
inline void StFgtA2CMaker::setPedestalFile(const Char_t* filename ){ mPedFilename = filename; };
inline void StFgtA2CMaker::setStatusFile(const Char_t* filename ){ mStatusFilename = filename; };

#endif
