#ifndef StCtbUtility_h
#define StCtbUtility_h
/*********************************************************************
 * $Id: StCtbUtility.h,v 1.4 2017/05/12 18:37:23 smirnovd Exp $
 *********************************************************************
 * utility function for CTB, used by the vertex finder(s)
 */

#include <vector>
#include <St_base/StObject.h> // just to make 'vector' working, JB
#include "Star2Root/St_DataSet.h"
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
 public:
   StCtbUtility();
    static void ctb_get_slat_from_data(int slat, int tray, float & phiRad, float &eta);
    void collectCTBhitsMC(St_DataSet *gds);
    void collectCTBhitsData(StTriggerData *trgD);
    void printCtb();
    int NCtbMatches();
};
#endif


/*
 * $Log: StCtbUtility.h,v $
 * Revision 1.4  2017/05/12 18:37:23  smirnovd
 * Cosmetic changes
 *
 * Removed log messages from source files
 * Prefixed included headers with paths to respective modules
 *
 * Revision 1.3  2005/03/11 22:23:53  balewski
 * towards PPV
 *
 * Revision 1.2  2005/03/09 19:24:18  balewski
 * preparation for PPV vertex finder
 *
 * Revision 1.1  2004/07/24 02:57:40  balewski
 * clean up of ppLMV, CTB-util separated
 *
 *
 *********************************************************************/
