// $Id: StECalEnergyIter.h,v 1.6 2005/06/08 18:53:18 balewski Exp $

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
class StEEmcDbMaker;

class StECalEnergyIter {
 private:
  StMuEmcCollection *mEmCol;
  StEEmcDbMaker *mEEdb;
  int mdetector;
  int mNhits;
  int mIhits;
  int mSuppBad;
  static bool mIsSimu;
  inline StECalEnergyIter() { /* no-op */ }
 
 protected:
  // Protected method if any

 public: 
  StECalEnergyIter(StMuEmcCollection *, int, StEEmcDbMaker *, bool=true);
  bool next(float &e, int &adc, int &adclessped, int &sec, int &eta, 
	    int &phi, char &cdet);
  inline bool operator()(float &e, int &adc, int &adclessped, int &sec, 
			 int &eta, int &phi, char &cdet)
    { return next(e, adc, adclessped, sec, eta, phi, cdet); }
  inline static void DeclareSimu() { mIsSimu = true; }

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StECalEnergyIter.h,v 1.6 2005/06/08 18:53:18 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

};

#endif

