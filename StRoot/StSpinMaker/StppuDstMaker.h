/***************************************************************************
 *
 * $Id: StppuDstMaker.h,v 1.3 2002/06/24 13:22:59 akio Exp $
 * $Log: StppuDstMaker.h,v $
 * Revision 1.3  2002/06/24 13:22:59  akio
 * numerous bug fix & updates
 *
 * Revision 1.2  2002/02/11 20:30:48  akio
 * Many updates, including very first version of jet finder.
 *
 * Revision 1.1  2002/01/16 20:22:54  akio
 * First version
 *
 * 
 * Author: Akio Ogawa June 2001
 ***************************************************************************
 *
 * Description:  TTree uDst for spin-pp
 *
 ***************************************************************************/
#ifndef StppuDst_h
#define StppuDst_h
#include "StMaker.h"

#define _GEANT_
#define _BBC_data_
#define _FPD_data_
//#define _EMC_CLUSTERS_
//#define _EMC_POINTS_

class TFile;
class TTree;
class StppEvent;
class StppGeant;
class StBbcTriggerDetector;
class StFpdCollection;
class StEmcClusterCollection;
class StEmcPoint;
class StMuDst;

class StppuDstMaker : public StMaker {
public:
  StppuDstMaker(const Char_t *name="StppuDst");
  Int_t Init(const Char_t* filename="spinDst.root");
  Int_t Make();
  Int_t Finish(); 
  void setMuDst(StMuDst* p) {mudst=p;};

protected:
    
private:
  StMuDst*        mudst;        //!
  size_t          mGoodCounter; //!
  size_t          mBadCounter;  //!
  TFile           *m_outfile;   //!
  TTree           *ppuDst;      //!
  StppEvent       *ppEvent;     //!
#ifdef _GEANT_
  StppGeant       *ppGeant;     //!
#endif
#ifdef _BBC_data_
  StBbcTriggerDetector *bbc;    //!
#endif
#ifdef _FPD_data_
  StFpdCollection *fpd;         //!
#endif
#ifdef _EMC_CLUSTERS_
  StEmcClusterCollection* emcClusters[3];//!
#endif
#ifdef _EMC_POINTS_
  TClonesArray     *emcPoints;  //!
#endif
  Int_t            infoLevel;
  
  ClassDef(StppuDstMaker,0)
};
#endif
