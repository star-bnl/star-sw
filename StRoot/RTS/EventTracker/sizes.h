#ifndef __SIZES_H_
#define __SIZES_H_
// JML....



// These are scaled to a "first-pulser" event
//       ~5 times a "normal" pulser
//       
#define szGL3_default_mHits    1000000    // global
#define szGL3_default_mTracks  15000      // global

#define szSL3_default_mHits    100000     // 1 sector
#define szSL3_default_mTracks  1500       // 1 sector

#define szSECP_max             75000
#define szGTD_max              1000000    
#define szL3_max               (szGTD_max)

#define trackerLib_gl3_mHits   szGL3_default_mHits
#define trackerLib_gl3_mTracks szGL3_default_mTracks

#define trackerLib_sl3_mHits   szSL3_default_mHits     // 1 sector
#define trackerLib_sl3_mTracks szSL3_default_mTracks

#endif
