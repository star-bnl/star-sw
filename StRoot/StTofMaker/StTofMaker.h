/***************************************************************************
 *
 * $Id: StTofMaker.h,v 1.4 2001/10/09 03:06:39 geurts Exp $ 
 * 
 * Author: Wei-Ming Zhang / Frank Geurts
 *
 ***************************************************************************
 *
 * Description: TOF offline software
 *              StTofMaker.h - ROOT/STAR Maker for offline chain.
 ***************************************************************************
 *
 * $Log: StTofMaker.h,v $
 * Revision 1.4  2001/10/09 03:06:39  geurts
 * TofTag introduced
 *
 * Revision 1.3  2001/09/28 18:40:03  llope
 * first release
 *
 *
 **************************************************************************/
#ifndef STAR_StTofMaker
#define STAR_StTofMaker
#include "StMaker.h"

//#define TOFP_DEBUG

// forward declarations
class StTofCollection;
class StTofSlat;
class StTofHit;
class StTofData;
class TH1F;
class StEvent;
class StTofSlatCollection;
class StTofHitCollection;
class StTofDataCollection;
class StTofPidTraits;
class StTofGeometry;
class StDAQReader;
class StTofReaderInterface;

// class definition
class StTofMaker : public StMaker {
 private:
  StDAQReader*           mTheDataReader; //!
  StTofReaderInterface*  mTheTofReader;  //!
  St_DataSet*            mTheTofData;    //!
  Bool_t               drawinit;         //!
  StEvent*             mEvent;           //!
  StTofGeometry*       mGeomDb;          //!
  StTofCollection*     mTheTofCollection;//!
  StTofSlatCollection* mSlatCollection;  //!
  StTofHitCollection*  mHitCollection;   //!
  StTofDataCollection* mDataCollection;  //!
  Int_t                tofTag; //!

  // flags
  short mTofCollectionPresent; //!
  short mSlatCollectionPresent;//!
  short mHitCollectionPresent; //!
  short mDataCollectionPresent;//!

  void fillStEvent();     //! ship collection to StEvent
  void storeTag();     //!
  void fillPidTraits();   //! method for testing classes of StTofPidMaker 

protected:
  TH1S *nAdcHitHisto;   //!
  TH1S *nTdcHitHisto;   //!

public: 
  StTofMaker(const char *name="tof");
  virtual ~StTofMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
    
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StTofMaker.h,v 1.4 2001/10/09 03:06:39 geurts Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StTofMaker, 1)   //StAF chain virtual base class for Makers
};
#endif
