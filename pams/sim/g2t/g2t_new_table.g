* $Id: g2t_new_table.g,v 1.2 2001/07/05 19:34:14 nevski Exp $
*
* $Log: g2t_new_table.g,v $
* Revision 1.2  2001/07/05 19:34:14  nevski
* all calorimeter use emc type table, pmd added
*
* Revision 1.1  2000/01/23 19:46:49  nevski
* *** empty log message ***
*
*
      FUNCTION   G2T_NEW_TABLE (name,spec,L)
*  make a correspondance between a spec and its include file 
      implicit   none
#include "g2t_hits.inc"
#include "g2t_svt_hit.inc"
#include "g2t_tpc_hit.inc"
#include "g2t_mwc_hit.inc"
#include "g2t_ctf_hit.inc"
#include "g2t_emc_hit.inc"
#include "g2t_ftp_hit.inc"
#include "g2t_pmd_hit.inc"
#include "g2t_vpd_hit.inc"
      character  name*(*),spec*(*),o*1
      integer    G2T_NEW_TABLE,TDM_NEW_TABLE,i,L 

      i = 0
      o = char(0)  
      if (spec=='svt') { i=TDM_NEW_TABLE (name, g2t_svt_hit_spec//o, L) } 
  elseif (spec=='tpc') { i=TDM_NEW_TABLE (name, g2t_tpc_hit_spec//o, L) } 
  elseif (spec=='mwc') { i=TDM_NEW_TABLE (name, g2t_mwc_hit_spec//o, L) } 
  elseif (spec=='ctf') { i=TDM_NEW_TABLE (name, g2t_ctf_hit_spec//o, L) } 
  elseif (spec=='emc') { i=TDM_NEW_TABLE (name, g2t_emc_hit_spec//o, L) } 
  elseif (spec=='smd') { i=TDM_NEW_TABLE (name, g2t_emc_hit_spec//o, L) } 
  elseif (spec=='eem') { i=TDM_NEW_TABLE (name, g2t_emc_hit_spec//o, L) } 
  elseif (spec=='esm') { i=TDM_NEW_TABLE (name, g2t_emc_hit_spec//o, L) } 
  elseif (spec=='ftp') { i=TDM_NEW_TABLE (name, g2t_ftp_hit_spec//o, L) } 
  elseif (spec=='vpd') { i=TDM_NEW_TABLE (name, g2t_vpd_hit_spec//o, L) }
  elseif (spec=='pmd') { i=TDM_NEW_TABLE (name, g2t_pmd_hit_spec//o, L) }
  else                 { i=TDM_NEW_TABLE (name, g2t_hits_spec//o,    L) }
  G2T_NEW_TABLE=i
  end

