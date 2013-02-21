/*!
 *
 * \class StSpinInfo_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Storage class of the basic spin information needed for
 * spin-dependent analyses.
 *
 */

#ifndef _ST_SPIN_INFO_H_
#define _ST_SPIN_INFO_H_

#include <TObject.h>


class StSpinInfo_t : public TObject {
 public:

   /// Construtor
   StSpinInfo_t();

   /// Deconstructor
   virtual ~StSpinInfo_t(){ /* */ };

   /// Clear function
   void clear();

   enum PolarizationType_t { UNDEFINED_POLARIZATION, LONG_LONG_POLARIZATION, TRANS_TRANS_POLARIZATION };

   /// Accessors
   Bool_t getValidDB() const { return mValidDB; };
   UShort_t getSpin4() const { return mSpin4; };
   UShort_t getBunchCrossing7bit() const { return mBunchCrossing7bit; };
   UShort_t getBunchCrossing48bit() const { return mBunchCrossing48bit; };
   UShort_t getBunchCrossingStar() const { return mBunchCrossingStar; };
   UShort_t getPolarizationType() const { return mPolarizationType; };
   UShort_t getDsmVertex() const { return mDsmVertex; };
   Bool_t bXingIsMaskedInSpinDB() const { return mbXingIsMasked; };

   /// Modifiers
   void setValidDB( Bool_t ValidDB ){ mValidDB = ValidDB; };
   void setSpin4( UShort_t Spin4 ){ mSpin4 = Spin4; };
   void setBunchCrossing7bit( UShort_t BunchCrossing7bit ){ mBunchCrossing7bit = BunchCrossing7bit; };
   void setBunchCrossing48bit( UShort_t BunchCrossing48bit ){ mBunchCrossing48bit = BunchCrossing48bit; };
   void setBunchCrossingStar( UShort_t BunchCrossingStar ){ mBunchCrossingStar = BunchCrossingStar; };
   void setPolarizationType( PolarizationType_t PolarizationType ){ mPolarizationType = PolarizationType; };
   void setDsmVertex( UShort_t DsmVertex ){ mDsmVertex = DsmVertex; };
   void setbXingIsMaskedInSpinDB( Bool_t itIs = 1 ){ mbXingIsMasked = itIs; };


 protected:
   Bool_t    mValidDB;             //
   Bool_t    mbXingIsMasked;       //
   UShort_t  mSpin4;               //
   UShort_t  mBunchCrossing7bit;   //
   UShort_t  mBunchCrossing48bit;  //
   UShort_t  mBunchCrossingStar;   //
   UShort_t  mDsmVertex;           //

   PolarizationType_t  mPolarizationType;    //

 private:
   ClassDef( StSpinInfo_t, 3 );
};

#endif

/*
 * $Id: StSpinInfo.h,v 1.2 2013/02/21 21:57:39 sgliske Exp $
 * $Log: StSpinInfo.h,v $
 * Revision 1.2  2013/02/21 21:57:39  sgliske
 * added mask field
 *
 * Revision 1.1  2012/11/26 19:03:06  sgliske
 * moved from offline/users/sgliske/StRoot/StSpinPool/StSpinInfo to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
