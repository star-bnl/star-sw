// $Id: StECalEnergyIter.h,v 1.1 2004/06/02 22:22:58 cadman Exp $

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
  inline StECalEnergyIter() { /* no-op */ }
 
 protected:
  // Protected method if any

 public: 
  StECalEnergyIter(StMuEmcCollection *emCol, int det, StEEmcDbMaker *eedb);
  bool next(float &e, int &adc, int &adclessped, int &sec, int &eta, 
	    int &phi, char &cdet);
  inline bool operator()(float &e, int &adc, int &adclessped, int &sec, 
			 int &eta, int &phi, char &cdet)
    { return next(e, adc, adclessped, sec, eta, phi, cdet); }

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StECalEnergyIter.h,v 1.1 2004/06/02 22:22:58 cadman Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

};

#endif

