**:>--------------------------------------------------------------------
**    module    G2T is the g2t table formatter only
**    author    Pavel Nevski
**    created   22 april 98
**:<--------------------------------------------------------------------
      subroutine   g2t 
      implicit     none
#include "g2t_event.inc"
#include "g2t_svt_hit.inc"
#include "g2t_tpc_hit.inc"
#include "g2t_mwc_hit.inc"
#include "g2t_ctf_hit.inc"
#include "g2t_emc_hit.inc"
#include "g2t_ftp_hit.inc"
#include "g2t_vpd_hit.inc"
      Integer   TDM_MAP_TABLE,DUI_CDIR,i
      External  TDM_MAP_TABLE,DUI_CDIR
      Record    /G2T_EVENT_ST/  g2t_event
      Character*1  o
      character*16 dir
      o   = char(0)
      dir ='/dui/Event'//o

      Call AGSTRUT(' ',' ')
      i =  TDM_map_table(dir,'g2t_event'//o,g2t_event_spec//o,1,g2t_event)
      i =  DUI_CDIR (dir)
      call TDM_NEW_TABLE ('g2t_svt_hit'//o, g2t_svt_hit_spec//o, 1) 
      call TDM_NEW_TABLE ('g2t_tpc_hit'//o, g2t_tpc_hit_spec//o, 1)
      call TDM_NEW_TABLE ('g2t_mwc_hit'//o, g2t_mwc_hit_spec//o, 1)
      call TDM_NEW_TABLE ('g2t_tof_hit'//o, g2t_ctf_hit_spec//o, 1)
      call TDM_NEW_TABLE ('g2t_ctb_hit'//o, g2t_ctf_hit_spec//o, 1)
      call TDM_NEW_TABLE ('g2t_emc_hit'//o, g2t_emc_hit_spec//o, 1)
      call TDM_NEW_TABLE ('g2t_smd_hit'//o, g2t_emc_hit_spec//o, 1)
      call TDM_NEW_TABLE ('g2t_eem_hit'//o, g2t_emc_hit_spec//o, 1)
      call TDM_NEW_TABLE ('g2t_esm_hit'//o, g2t_emc_hit_spec//o, 1)
      call TDM_NEW_TABLE ('g2t_ftp_hit'//o, g2t_ftp_hit_spec//o, 1)
      call TDM_NEW_TABLE ('g2t_vpd_hit'//o, g2t_vpd_hit_spec//o, 1)
      
      print *,' *********** g2t main called *********** '
      call g2tmain
      i =  DUI_CDIR ('/dui'//o)
      end


**:>--------------------------------------------------------------------
module    G2Tmain  is g2t converter
author    Pavel Nevski
created   22 april 98
**:<--------------------------------------------------------------------
#include "PAM.inc"
#include "geant321/gcnum.inc"
#include "g2t_vertex.inc"
#include "g2t_track.inc"
#include "g2t_hits.inc"
*
      structure gtot { int version, int nsys }
      structure dete { int Isys, char ctab, char csys, char cdet }
      Integer        G2T_MAIN,Iprin
      Integer        AgFHIT0,Istat/-1/
      Integer        TDM_NEW_TABLE,AMI_CALL,TDM_FIND_SPEC,AMI_MODULE_CALL
      External       TDM_NEW_TABLE,AMI_CALL,TDM_FIND_SPEC,AMI_MODULE_CALL
      Integer        i,j,nhits,nnhits
      Character*16   ctab,ctabo,names(3)
      Character*1    o
      Integer        Lout/6/
*
*
If (First) Then
   first = .false.
   fill gtot(1)       ! g2t control
     version = 1         ! version number
     nsys    = 15        ! number of subsystems
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'svt'        ! table name
     csys = 'SVTT'       ! Geant Subsystem
     cdet = 'SVTD'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'tpc'        ! table name
     csys = 'TPCE'       ! Geant Subsystem
     cdet = 'TPAD'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'tpc'        ! table name
     csys = 'TPCE'       ! Geant Subsystem
     cdet = 'TPAI'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'tpc'        ! table name
     csys = 'TPCE'       ! Geant Subsystem
     cdet = 'TPAO'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'mwc'        ! table name
     csys = 'TPCE'       ! Geant Subsystem
     cdet = 'TMSE'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'ctb'        ! table name
     csys = 'BTOF'       ! Geant Subsystem
     cdet = 'BCSA'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'tof'        ! table name
     csys = 'BTOF'       ! Geant Subsystem
     cdet = 'BCSB'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'emc'        ! table name
     csys = 'CALB'       ! Geant Subsystem
     cdet = 'CSUP'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'smd'        ! table name
     csys = 'CALB'       ! Geant Subsystem
     cdet = 'CSDA'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'eem'        ! table name
     csys = 'ECAL'       ! Geant Subsystem
     cdet = 'ESCI'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'esm'        ! table name
     csys = 'ECAL'       ! Geant Subsystem
     cdet = 'MSEC'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'ftp'        ! table name
     csys = 'FTPC'       ! Geant Subsystem
     cdet = 'FSEN'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'vpd'        ! table name
     csys = 'VPDD'       ! Geant Subsystem
     cdet = 'VRAD'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'psc'        ! table name
     csys = 'PMDD'       ! Geant Subsystem
     cdet = 'VRAD'       ! Sensitive detector
   fill dete       ! star subsystem
     Isys = 1            ! system number
     ctab = 'pgc'        ! table name
     csys = 'PMDD'       ! Geant Subsystem
     cdet = 'VRAD'       ! Sensitive detector
   endfill
*
endif
*
      G2T_MAIN = STAFCV_BAD
      o = CHAR(0)
      Use /detm/g2tm/gtot

      i = TDM_NEW_TABLE ('g2t_vertex'//o,G2T_VERTEX_SPEC//o,NVERTX)
      i = TDM_NEW_TABLE ('g2t_track'//o, G2T_TRACK_SPEC//o, NTRACK)

      names(1)='g2t_vertex'//o
      names(2)='g2t_track'//o
      i = AMI_MODULE_CALL ('g2t_get_kine'//o,2,names)
      prin3 i; (' g2tmain: get_kine done with i=',i6)

      do j=1,gtot_Nsys

        use dete(j)  Stat=Istat
        if (Istat!=ok) break
        ctab='g2t_'//dete_ctab(1:3)//'_hit'//Char(0)

        Check agfhit0 (dete_Csys,dete_Cdet) == ok
        call  GFNHIT  (dete_Csys(1:3)//'H',dete_Cdet,Nhits)
        Check Nhits>0

        prin1 ctab,dete_Csys,dete_Cdet,nhits
        (' in G2Tmain: found ',3(1x,a),'   Nhits = ',i6)

        if (ctabo .ne. ctab ) nnhits = 0
        ctabo  = ctab
        nnhits = nnhits + Nhits
     
        i = TDM_FIND_SPEC(ctab)
        if (i.gt.0) then
           i = TDM_NEW_TABLE (ctab, ctab, nnhits)
        else
           i = TDM_NEW_TABLE (ctab, G2T_HITS_SPEC//o,nnhits)
        endif
        prin3 i; (' g2tmain ===> tdm table for g2t_hits = ',i6)
        
        names(3)=ctab
        i = AMI_CALL ('g2t_get_hits'//o,3,names)
        prin3 i; (' g2tmain ===> ami_call g2t_get_hits  = ',i6)

      enddo
      G2T_MAIN = STAFCV_OK
      END
