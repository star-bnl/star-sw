// $Id: StSsdDaqMaker.h,v 1.1 2005/04/15 15:11:21 lmartin Exp $
//
// $Log: StSsdDaqMaker.h,v $
// Revision 1.1  2005/04/15 15:11:21  lmartin
// StSsdDaqMaker
//
#ifndef STAR_StSsdDaqMaker
#define STAR_StSsdDaqMaker

/*!
 *                                                                     
 * \class  StSsdDaqMaker
 * \author Martin,Reinnarth,Bouchet
 * \date   2004/11/03
 * \brief  Class to read SSD data from DAQ files
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 * 
 * StSsdDaqMaker Class to read SSD data from DAQ files 
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif


// You may forward declare other classes if you have data-members
// used in pointer-only context by using declaration like
// class St_SomeExternClass;
//
// You do need in such simple case to add the include file
// (and compilation is much faster).
class StDAQReader;
class StSsdConfig;
class St_ssdConfiguration;
class ssdConfiguration_st;
class St_spa_strip;
class St_ssdPedStrip;

class StSsdDaqMaker : public StMaker {
 private:
  // Private method declaration if any
  // St_spa_strip   *spa_strip;
  // St_ssdPedStrip *ssdPedStrip;
 
 protected:
  // Protected method if any
  StSsdConfig*  mConfig;

 public: 
  StSsdDaqMaker(const char *name="StSsdDaqMaker");
  virtual       ~StSsdDaqMaker();
  virtual Int_t Init();
  virtual Int_t  Make();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StSsdDaqMaker.h,v 1.1 2005/04/15 15:11:21 lmartin Exp $ built "__DATE__" "__TIME__; 
    return cvs;
  }

  ClassDef(StSsdDaqMaker,0)   //StAF chain virtual base class for Makers
};

#endif


