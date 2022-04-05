// $Id: StECalEnergyIter.h,v 1.8 2014/08/06 11:42:58 jeromel Exp $

#ifndef STAR_StECalEnergyIter
#define STAR_StECalEnergyIter

/*!
 *                                                                     
 * \class  StECalEnergyIter
 * \author cadman
 * \date   2004/05/27
 * \brief  Iterate over hits and extract energy using database
 *
 *
 *
 */                                                                      


class StMuEmcCollection;
class StEEmcDb;

class StECalEnergyIter {
 private:
  StMuEmcCollection *mEmCol;
  StEEmcDb *mEEdb;
  int mdetector;
  int mNhits;
  int mIhits;
  int mSuppBad;
  static bool mIsSimu;
  inline StECalEnergyIter() { /* no-op */ }
 
 protected:
  // Protected method if any

 public: 
  StECalEnergyIter(StMuEmcCollection *, int, StEEmcDb *db, bool=true);
  bool next(float &e, int &adc, int &adclessped, int &sec, int &eta, 
	    int &phi, char &cdet);
  inline bool operator()(float &e, int &adc, int &adclessped, int &sec, 
			 int &eta, int &phi, char &cdet)
    { return next(e, adc, adclessped, sec, eta, phi, cdet); }
  inline static void DeclareSimu() { mIsSimu = true; }

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StECalEnergyIter.h,v 1.8 2014/08/06 11:42:58 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

};

#endif

