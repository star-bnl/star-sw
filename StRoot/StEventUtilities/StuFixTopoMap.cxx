/***************************************************************************
 *
 * $Id: StuFixTopoMap.cxx,v 1.13 2018/10/16 19:36:49 genevb Exp $
 *
 * Author: Thomas Ullrich, May 2000
 ***************************************************************************
 *
 * Description:  StuFixTopoMap rebuilds the StTrackTopologyMap
 *               stored for every track. It assumes that each
 *               track references a valid instance of StTrackDetectorInfo
 *               from which it takes the information to re-create
 *               the map.
 *
 * Syntax: bool StuFixTopoMap(StTrack*);
 *
 *               Returns true if the topology map was rebuild
 *               successfully.
 *
 * The description on the format of the map as taken from the
 * dst_track.idl file reads:
 *
 *    SVT and/or SSD and/or TPC Tracks                               
 *    --------------------------------                               
 *    map[0]   Bit number  Quantity                                  
 *    ------   ----------  --------                                  
 *                 0       Primary Vertex used(1) or not(0);         
 *                         also used to indicate that a secondary    
 *                         vertex constraint was used for special    
 *                         track fitting methods for decay vertices  
 *                         (see method variable).                    
 *                1-6      SVT 3 superlayers, inner/outer            
 *                 7       SSD 4th layer                             
 *                8-31     TPC, first 24 padrows                     
 *                                                                   
 *    map[1]   Bit number  Quantity                                  
 *    ------   ----------  --------                                  
 *                0-20     TPC, remaining 21 padrows                 
 *                21       Track extrapolates to MWC (no=0, yes=1)   
 *                22       Track extrapolates to CTB (no=0, yes=1)   
 *                23       Track extrapolates to TOF (no=0, yes=1)   
 *                24       Track extrapolates to RCH (no=0, yes=1)   
 *                25       Track extrapolates to EMCB (no=0, yes=1)  
 *                26       Track extrapolates to EMCEC (no=0, yes=1) 
 *               27-29     reserved for future use                   
 *                30       Turn around flag, some elements used >1   
 *                31       Format interpreter; (SVT/SSD/TPC=0,FTPC=1)
 *
 *    HFT and/or TPC Tracks
 *    --------------------------------
 *    map[0]   Bit number  Quantity
 *    ------   ----------  --------
 *                 0       Primary Vertex used(1) or not(0);
 *                         also used to indicate that a secondary
 *                         vertex constraint was used for special
 *                         track fitting methods for decay vertices
 *                         (see method variable).
 *                1-3      PXL 2 layers, possible 2 hits on the second layer (overlapping ladders)
 *                4-5      IST layer, possible 2 hits
 *                6-7      SSD layer, possible 2 hits
 *                8-31     TPC, first 24 padrows
 *
 *    map[1]   Bit number  Quantity
 *    ------   ----------  --------
 *                0-20     TPC, remaining 21 padrows
 *                21       Track extrapolates to MWC (no=0, yes=1)
 *                22       Track extrapolates to CTB (no=0, yes=1)
 *                23       Track extrapolates to TOF (no=0, yes=1)
 *                24       Track extrapolates to RCH (no=0, yes=1)
 *                25       Track extrapolates to EMCB (no=0, yes=1)
 *                26       Track extrapolates to EMCEC (no=0, yes=1)
 *               27-28     reserved for future use
 *                29       HFT flag  (HFT=1, SVT/SSD=0)
 *                30       Turn around flag, some elements used >1
 *                31       Format interpreter; (SVT/SSD/TPC=0,FTPC=1)
 *
 *                                                                   
 *    FTPC Tracks                                                    
 *    -----------                                                    
 *    map[0]   Bit number  Quantity                                  
 *    ------   ----------  --------                                  
 *                 0       Primary Vertex used or not                
 *                1-10     FTPC East pad rows, increasing away from  
 *                         STAR coord. sys. origin at (0,0,0)        
 *                11-20    FTPC West pad rows, increasing away from  
 *                         STAR coord. sys. origin at (0,0,0)        
 *                21-31    not used; for future use                  
 *                                                                   
 *    map[1]   Bit number  Quantity                                  
 *    ------   ----------  --------                                  
 *                0-26     not used; for future use                  
 *               27-29     reserved for future use                   
 *                30       Turn around flag, some elements used >1   
 *                31       Format interpreter; (SVT/SSD/TPC=0,FTPC=1)
 *
 ***************************************************************************
 *
 * $Log: StuFixTopoMap.cxx,v $
 * Revision 1.13  2018/10/16 19:36:49  genevb
 * Remove erroneous previous commit changes, and add flexibility for iTpc hit determination
 *
 *
 **************************************************************************/
#include "StEventTypes.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"


bool StuFixTopoMap(StTrack* track)
{
    if (!track) return false;
    
    const StTrackDetectorInfo *info = track->detectorInfo();
    if (!info) return false;
    
    const StPtrVecHit& hits = info->hits();
    
    unsigned long word1 = 0;
    unsigned long word2 = 0;
    unsigned long long wordiTpc = 0;
    
    // Primary vertex used or not
    if (track->type() == primary) word1 |= 1U;
    
    //
    // Different coding for FTPC and TPC/SVT/SSD tracks
    //
    unsigned int i, k;
    if (info->numberOfReferencedPoints(kFtpcWestId) ||
        info->numberOfReferencedPoints(kFtpcEastId)) {
        
        //
        // FTPC
        //
        word2 |= 1U<<31; 
        for (i=0; i<hits.size(); i++) {
            if (hits[i]->detector() == kFtpcEastId ||
                hits[i]->detector() == kFtpcEastId) {
                k = dynamic_cast<const StFtpcHit*>(hits[i])->plane();
                if (word1 & 1U<<k) word2 |= 1U<<30; // turnaround flag    
                word1 |= 1U<<k;
            }
        }
    }
    else {

        // ???
        if (info->numberOfReferencedPoints(kPxlId) ||
            info->numberOfReferencedPoints(kSstId) ||
            info->numberOfReferencedPoints(kIstId)) {
          LOG_DEBUG<<" HFT track!" << endm;
          word2 |= 1U<<29;
        }
        
        //
        // TPC/SVT/SSD or TPC/SSD/IST/PXL
        //

        for (i=0; i<hits.size(); i++) {
            if (hits[i]->detector() == kSvtId) {
                k = dynamic_cast<const StSvtHit*>(hits[i])->layer();
                if (word1 & 1U<<k) word2 |= 1U<<30; // turnaround flag    
                word1 |= 1U<<k;
            }
            else if (hits[i]->detector() == kPxlId) {
                k = dynamic_cast<const StPxlHit*>(hits[i])->layer();       
                LOG_DEBUG<<"track has hit in pixel detector, layer "<<k<<endm;
                if (k==2 && (word1 & 1U<<k)) {
                  word1 |= 1U<<(k+1);  // second layer, possible has two hits
                  LOG_DEBUG<<"set bit for PXL 3rd-layer to 1"<<endm;    
                } else {
                  word1 |= 1U<<k;
                  LOG_DEBUG<<"set bit for PXL "<<k<<"th-layer to 1"<<endm;
                }
                LOG_DEBUG<<"word1: "<<word1<<endm;
            }
            else if (hits[i]->detector() == kIstId) {
                LOG_DEBUG<<"track has hit in ist" << endm;
                if (word1 & 1U<<4) { 
                  word1 |= 1U<<5;  // second hit
                  LOG_DEBUG<<"set bit for IST 2nd-layer to 1"<<endm;
                } else {
                  word1 |= 1U<<4;
                  LOG_DEBUG<<"set bit for IST 1st-layer to 1"<<endm;
                }
                LOG_DEBUG<<"word1: "<<word1<<endm;
            }
            else if (hits[i]->detector() == kSsdId ||
                     hits[i]->detector() == kSstId) {
                if(word2 & 1U<<29) { // HFT format
                  LOG_DEBUG<<"HFT format for the SST hit map"<<endm;
                  if (word1 & 1U<<6) {
                    word1 |= 1U<<7;  // second hit
                    LOG_DEBUG<<"set bit for SST 2nd-layer to 1"<<endm;
                  } else {
                    word1 |= 1U<<6;
                    LOG_DEBUG<<"set bit for SST 1st-layer to 1"<<endm;
                  }
                } else {
                  if (word1 & 1U<<7) word2 |= 1U<<30; // turnaround flag    
                  LOG_DEBUG<<"track has hit in ssd"<<endm;
                  word1 |= 1U<<7;
                }
                LOG_DEBUG<<"word1: "<<word1<<endm;
            }
            else if (hits[i]->detector() == kTpcId) {
                unsigned int sector = static_cast<const StTpcHit*>(hits[i])->sector();
                unsigned int padrow = static_cast<const StTpcHit*>(hits[i])->padrow();
                bool isiTpc = St_tpcPadConfigC::instance()->isiTpcSector(sector);
                bool isInner = St_tpcPadConfigC::instance()->isInnerPadRow(sector, padrow);
                if (isiTpc && isInner) {
                    // Need to treat as iTPC even if not using kiTpcId
                    k = dynamic_cast<const StTpcHit*>(hits[i])->padrow();
                    if (wordiTpc & 1ULL<<k) word2 |= 1U<<30; // turnaround flag    
                    wordiTpc |= 1ULL<<k;
                    continue;
                }
                k = isiTpc ? padrow - 40 + 13 : padrow; //outer rows of iTPC sector
                if (k < 25) {
                    if (word1 & 1U<<(k+7)) word2 |= 1U<<30; // turnaround flag    
                    word1 |= 1U<<(k+7);
                }
                else {
                    if (word2 & 1U<<(k-25)) word2 |= 1U<<30; // turnaround flag    
                    word2 |= 1U<<(k-25);
                }
            }
            else if (hits[i]->detector() == kiTpcId) {
                k = dynamic_cast<const StTpcHit*>(hits[i])->padrow();
                if (wordiTpc & 1ULL<<k) word2 |= 1U<<30; // turnaround flag    
                wordiTpc |= 1ULL<<k;
            }		
        }	
    }
    
    StTrackTopologyMap newmap(word1, word2, wordiTpc);
    track->setTopologyMap(newmap);
    return true;
}
