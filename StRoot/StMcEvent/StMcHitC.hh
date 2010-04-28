/***************************************************************************
 *
 * $Id: StMcHitC.hh,v 2.1 2010/04/28 18:10:10 fine Exp $
 * $Log: StMcHitC.hh,v $
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
//#include "tables/St_g2t_hits_Table.h"
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
ENDMCHITCLASS

MCHITCLASS(StMcFgtHitC,g2t_fgt_hit_st) 
ENDMCHITCLASS

MCHITCLASS(StMcFtpcHitC,g2t_ftp_hit_st) 
ENDMCHITCLASS

MCHITCLASS(StMcIstHitC,g2t_ist_hit_st)
ENDMCHITCLASS

MCHITCLASS(StMcPixHitC,g2t_pix_hit_st)
ENDMCHITCLASS

MCHITCLASS(StMcRchHitC,g2t_rch_hit_st)
ENDMCHITCLASS

MCHITCLASS(StMcSsdHitC,g2t_ssd_hit_st)
ENDMCHITCLASS

MCHITCLASS(StMcSvtHitC,g2t_svt_hit_st)
ENDMCHITCLASS

MCHITCLASS(StMcTpcHitC,g2t_tpc_hit_st)
ENDMCHITCLASS

MCHITCLASS(StMcHitC,g2t_hits_st)
ENDMCHITCLASS

#endif

