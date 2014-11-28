// $Id: StEandBDirMaker.h,v 1.3 2013/08/26 16:51:59 fisyak Exp $

#ifndef STAR_StEandBDirMaker
#define STAR_StEandBDirMaker

/*!
 *                                                                     
 * \class  StEandBDirMaker
 * \author fisyak
 * \date   2013/08/14
 * \brief  Try to measure direction Electric and Magnetic Field using low energy elctrons
 *
 */                                                                      
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StThreeVectorD.hh"
class Tracklet : public TObject {
public:
  Tracklet() {Clear();}
  ~Tracklet() {}
  Char_t         beg[1]; // !
  Int_t          run;
  Int_t          sector;
  Int_t          row;
  Int_t          nhits;
  Double_t       AdcSum;
  Double_t       x0, tX, y0, tY;
  Double_t       x0T, tXT, y0T, tYT; // in Tpc coordinate system
  Char_t         end[1]; // !
  StThreeVectorD BG;
  StThreeVectorD BL;
  StThreeVectorD BT;
  StThreeVectorD posG;
  StThreeVectorD posL;
  StThreeVectorD dirL;
  StThreeVectorD posT;
  StThreeVectorD dirT;
  StThreeVectorD dirST;
  StThreeVectorD posRMSG;
  StThreeVectorD posRMSL;
  StThreeVectorD posRMST;
  void           Clear(Option_t *opt = 0) {
    if (opt); memset(beg, 0, end-beg); 
    BG = BL = posG = posL = posT = posRMSG = posRMSL = posRMST = dirL = dirT = StThreeVectorD();
  }
  ClassDef(Tracklet,1)
};

class StEandBDirMaker : public StMaker {
 private:
  // Private method declaration if any
  Tracklet *fTracklet;
 protected:
  // Protected method if any
 public: 
  StEandBDirMaker(const char *name="EandBDir") : StMaker(name){}
  virtual ~StEandBDirMaker() {}
  virtual Int_t Init();
  virtual Int_t Make();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  // Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEandBDirMaker.h,v 1.3 2013/08/26 16:51:59 fisyak Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  ClassDef(StEandBDirMaker,0)   //
};
#endif
// $Log: StEandBDirMaker.h,v $
// Revision 1.3  2013/08/26 16:51:59  fisyak
// Add rms
//
// Revision 1.2  2013/08/19 20:25:31  fisyak
// Add dirST
//
// Revision 1.1.1.1  2013/08/16 15:27:45  fisyak
// First version
//
