#ifndef StCtbUtility_h
#define StCtbUtility_h
/*********************************************************************
 * $Id: StCtbUtility.h,v 1.1 2004/07/24 02:57:40 balewski Exp $
 *********************************************************************
 * utility function for CTB, used by the vertex finder(s)
 */

#include <vector>
#include <StObject.h> // just to make 'vector' working, JB
#include "St_DataSet.h"
class StTriggerData;

class StCtbUtility {
 protected:
  //data
    float  mCtbEtaSeg, mCtbPhiSeg;
    
   /*!
     * \cuts
     */

    float mCtbThres_mev;
    int mCtbThres_ch;// to reject slats below threshold
    
     /*!
     * \struct ctbHit 
     */
    struct ctbHit {
      float phi; // (rad)
      float eta; 
      float adc;
    };
    vector<ctbHit> mCtbHits;

    // methods
   StCtbUtility();
    void ctb_get_slat_from_data(int slat, int tray, float & phiRad, float &eta);
    void collectCTBhitsMC(St_DataSet *gds);
    void collectCTBhitsData(StTriggerData *trgD);
    int NCtbMatches();
};
#endif


/*
 * $Log: StCtbUtility.h,v $
 * Revision 1.1  2004/07/24 02:57:40  balewski
 * clean up of ppLMV, CTB-util separated
 *
 *
 *********************************************************************/
