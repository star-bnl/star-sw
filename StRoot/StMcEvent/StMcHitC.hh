/***************************************************************************
 *
 * $Id: StMcHitC.hh,v 2.2 2010/04/28 20:15:45 fine Exp $
 * $Log: StMcHitC.hh,v $
 * Revision 2.2  2010/04/28 20:15:45  fine
 * Implementation if the new OO for Mc hits
 *
 * Revision 2.1  2010/04/28 18:10:10  fine
 * New OO model for Mc event components
 *
 * Revision 2.10  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
  *
 *
 **************************************************************************/
#ifndef StMcHitC_hh
#define StMcHitC_hh

#include "StEventObject.h"
#include "Stiostream.h"
#include "StThreeVectorF.hh"

#include "g2t_vpd_hit.h"
#include "g2t_ctf_hit.h"
#include "g2t_tpc_hit.h"
#include "g2t_emc_hit.h"
#include "g2t_ftp_hit.h"
#include "g2t_svt_hit.h"
#include "g2t_fgt_hit.h"
#include "g2t_hits.h"
#include "g2t_ist_hit.h"
#include "g2t_pix_hit.h"
#include "g2t_rch_hit.h"
#include "g2t_ssd_hit.h"
#include "g2t_pmd_hit.h"
#include "g2t_mwc_hit.h"
#include "g2t_fst_hit.h"
#include "g2t_gem_hit.h"
#include "g2t_hpd_hit.h"
#include "g2t_igt_hit.h"

#include "StMcHitT.hh"

MCHITCLASS(StMcCtfHitC,g2t_ctf_hit_st)
    void get_slat_tray(unsigned int & slat, unsigned int & tray) const;
ENDMCHITCLASS

MCHITCLASS(StMcFgtHitC,g2t_fgt_hit_st) 
    unsigned long layer() const;
    unsigned long quad() const;
ENDMCHITCLASS

MCHITCLASS(StMcFtpcHitC,g2t_ftp_hit_st) 
    unsigned long plane() const; // 1-20, where 1-10 = West and 11-20 = East
    unsigned long sector() const; // 1-6
ENDMCHITCLASS

MCHITCLASS(StMcIstHitC,g2t_ist_hit_st)
    unsigned long layer() const; // 
    unsigned long ladder() const; //    
    // Willie: Added function wafer() to return wafer number (1-12)
    unsigned long wafer() const;
ENDMCHITCLASS

MCHITCLASS(StMcPixelHitC,g2t_pix_hit_st)
    unsigned long layer() const; // 1-2
    unsigned long ladder() const; // 1-6, 1-18
ENDMCHITCLASS

MCHITCLASS(StMcRichHitC,g2t_rch_hit_st)
    unsigned short  pad() const { return (volumeId() & 0xff); }       // first 8 bits
    unsigned short  row() const {return ( (volumeId()>>8) & 0xff); }  // second 8 bits;
ENDMCHITCLASS

MCHITCLASS(StMcSsdHitC,g2t_ssd_hit_st)
  unsigned long ladder() const {return  volumeId()%100;     }
  unsigned long wafer()  const {return ((volumeId()-7000)/100)%100;}
ENDMCHITCLASS

MCHITCLASS(StMcSvtHitC,g2t_svt_hit_st)
    unsigned long layer() const;      // layer=[1,6] with SSD [1-8]
    unsigned long ladder() const;     // ladder=[1-8] with SSD [1-20]
    unsigned long wafer() const;      // wafer=[1-7] with SSD [1-16]
    unsigned long barrel() const;     // barrel=[1-3] with SSD [1-4]
    unsigned long hybrid() const;
ENDMCHITCLASS

MCHITCLASS(StMcTpcHitC,g2t_tpc_hit_st)
    unsigned long sector()     const; // 1-24
    unsigned long padrow()     const; // 1-45

    unsigned long isDet()      const { return volumeId()/100000;}
    float         lgamma()     const { return StEventObject<g2t_tpc_hit_st*>::fData->lgam;}
ENDMCHITCLASS

MCHITCLASS(StMcHitC,g2t_hits_st)
ENDMCHITCLASS

MCHITCLASS(StMcBTofHitC,g2t_ctf_hit_st)
  float sTrack() const{ return StEventObject<g2t_ctf_hit_st*>::fData->s_track;};
ENDMCHITCLASS

// It has no table inteface 
MCHITCLASS(StMcCalorimeterHitC,g2t_tpc_hit_st)
ENDMCHITCLASS

//_____________________________________________________________________________
//
//       Svt  hits
//_____________________________________________________________________________
inline unsigned long StMcSvtHitC::layer() const
{
    // Volume Id: 1000*layer + 100*wafer + ladder (Helen, Nov 99)
    return (volumeId())/1000;
}

inline unsigned long StMcSvtHitC::ladder() const
{
    // Volume Id: 1000*layer + 100*wafer + ladder (Helen, Nov 99)
    return volumeId()%100 ;
}

inline unsigned long StMcSvtHitC::wafer() const
{
    // Volume Id: 1000*layer + 100*wafer + ladder (Helen, Nov 99)

    if (volumeId() < 7101) // SVT
	return ((volumeId())%1000)/100;
    else // SSD
	return ((volumeId()/100)-70);
}

inline unsigned long StMcSvtHitC::barrel() const { return (layer()+1)/2; }

//_____________________________________________________________________________
//
//       Tpc  hits
//_____________________________________________________________________________
inline unsigned long StMcTpcHitC::sector() const
{
    // volume Id = 10000 * some junk + 100 * sector + padrow
    return (volumeId()%10000)/100;   //  sector=[1,24]
}

//_____________________________________________________________________________
inline unsigned long StMcTpcHitC::padrow() const
{
    return volumeId()%100;   // padrow=[1-45]
}

#endif

