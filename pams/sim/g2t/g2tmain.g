**:>-----------------------------------------------------------------------
module    G2Tmain  is g2t converter
author    Pavel Nevski
created   22 april 98
*
* description: create and fill two new directories in the CWD:
*              Run   - with tables mapped to DETM family banks
*              Event - with track/vertex and hit tables
*              One level is assumed, names can be modified by DETP command
**:<-----------------------------------------------------------------------
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gcflag.inc"
#include "g2t_event.inc"
#include "g2t_vertex.inc"
#include "g2t_track.inc"
#include "g2t_hits.inc"
+cde,quest.
*
      structure GTTC { int version, int nsys , char edir(3), char rdir(3) }
      structure dete { onoff, char ctab, char spec, char csys, char cdet }
      Integer        G2T_MAIN,AgFHIT0,DUI_CDIR,TDM_MAP_TABLE
      Integer        TDM_NEW_TABLE,AMI_CALL,G2T_NEW_TABLE,AMI_MODULE_CALL
      External       TDM_NEW_TABLE,AMI_CALL,G2T_NEW_TABLE,AMI_MODULE_CALL
      Integer        i,j,ld,nhits,nnhits,Iprin,Istat/-1/,Lout/6/
      Character*16   ctab,ctabo,names(3)
      Character      o*1,cdir*20,edir*20
      Record         /G2T_EVENT_ST/  g2t_event
*
 entry  g2t 
 entry  g2t_start
 begin 
 G2T_MAIN=0 
 if JVOLUM<=0 { print *,' G2T: no geometry loaded YET'; return; }
*
 If (First) Then
   first = .false.
   fill GTTC(1)           ! g2t control
     version = 1                  ! version number
     nsys    = 17                 ! number of subsystems
     edir    = {'Even','t',' '}   ! event output directory name 
     rdir    = {'Run ',' ',' '}   ! run output directory name 
   fill dete       ! star subsystem
     onoff = 1            ! system number
     ctab  = 'svt'        ! table name
     spec  = 'svt'        ! specification type
     csys  = 'SVTT'       ! Geant Subsystem
     cdet  = 'SVTD'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 2            ! system number
     ctab  = 'svt'        ! table name
     spec  = 'svt'        ! specification type
     csys  = 'SVTT'       ! Geant Subsystem
     cdet  = 'SFSD'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 3            ! system number
     ctab  = 'tpc'        ! table name
     spec  = 'tpc'        ! specification type
     csys  = 'TPCE'       ! Geant Subsystem
     cdet  = 'TPAD'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 4            ! system number
     cdet  = 'TPAI'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 5            ! system number
     cdet  = 'TPAO'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 6            ! system number
     ctab  = 'mwc'        ! table name
     spec  = 'mwc'        ! specification type
     csys  = 'TPCE'       ! Geant Subsystem
     cdet  = 'TMSE'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 7            ! system number
     ctab  = 'ctb'        ! table name
     spec  = 'ctf'        ! specification type
     csys  = 'BTOF'       ! Geant Subsystem
     cdet  = 'BCSA'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 8            ! system number
     ctab  = 'tof'        ! table name
     spec  = 'ctf'        ! specification type
     csys  = 'BTOF'       ! Geant Subsystem
     cdet  = 'BCSB'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 9            ! system number
     ctab  = 'emc'        ! table name
     spec  = 'emc'        ! specification type
     csys  = 'CALB'       ! Geant Subsystem
     cdet  = 'CSUP'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 10            ! system number
     ctab  = 'smd'        ! table name
     spec  = 'emc'        ! specification type
     csys  = 'CALB'       ! Geant Subsystem
     cdet  = 'CSDA'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 11           ! system number
     ctab  = 'eem'        ! table name
     spec  = 'emc'        ! specification type
     csys  = 'ECAL'       ! Geant Subsystem
     cdet  = 'ESCI'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 12           ! system number
     ctab  = 'esm'        ! table name
     spec  = 'emc'        ! specification type
     csys  = 'ECAL'       ! Geant Subsystem
     cdet  = 'MSEC'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 13           ! system number
     ctab  = 'ftp'        ! table name
     spec  = 'ftp'        ! specification type
     csys  = 'FTPC'       ! Geant Subsystem
     cdet  = 'FSEN'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 14           ! system number
     ctab  = 'ftp'        ! table name
     spec  = 'ftp'        ! specification type
     csys  = 'FTPC'       ! Geant Subsystem
     cdet  = 'FSEC'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 15           ! system number
     ctab  = 'vpd'        ! table name
     spec  = 'vpd'        ! specification type
     csys  = 'VPDD'       ! Geant Subsystem
     cdet  = 'VRAD'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 16           ! system number
     ctab  = 'psc'        ! table name
     spec  = '   '        ! specification type
     csys  = 'PMDD'       ! Geant Subsystem
     cdet  = 'VRAD'       ! Sensitive detector
   fill dete       ! star subsystem
     onoff = 17           ! system number
     ctab  = 'pgc'        ! table name
   endfill
*
 endif
*   
      G2T_MAIN = 0
      Use GTTC
*     map detm family banks:
      o    = CHAR(0)
      Cdir = gttc_rdir(1)//gttc_rdir(2)//gttc_rdir(3)
      Ld=0; Do i=1,12 { if (Cdir(i:i)==' '|Cdir(i:i)==o) Break; Ld=i; }
      If (Ld>0&Cdir!='none')  Call AGSTRUT (' ',Cdir(1:ld))
*
      Cdir = gttc_edir(1)//gttc_edir(2)//gttc_edir(3)
      Ld=0; Do i=1,12 { if (Cdir(i:i)==' '|Cdir(i:i)==o) Break; Ld=i; }
      edir = Cdir(1:ld)//o;  Call TDM_CLEAR_ALL(edir)

      IQUEST(1)=IEOTRI
      check NVERTX>0 & IEOTRI==0
*     map ghea_* headers and  hepe_* particle tables:
      call agstrut('/evnt/@HEPE',Edir)
      call agstrut('/evnt/@GHEA',Edir)
 
      i = TDM_map_table(edir,'g2t_event'//o,g2t_event_spec//o,1,g2t_event)
      if (ld>0) i = DUI_CDIR (edir)
      i = TDM_NEW_TABLE('g2t_vertex'//o,G2T_VERTEX_SPEC//o,NVERTX)
      i = TDM_NEW_TABLE('g2t_track'//o, G2T_TRACK_SPEC//o, NTRACK)

      names(1)='g2t_vertex'//o
      names(2)='g2t_track'//o

      i = AMI_MODULE_CALL ('g2t_get_kine'//o,2,names)
      prin5 i; (' g2tmain: get_kine done with i=',i6)

      Use GTTC
      do j=1,GTTC_Nsys

        use dete(j) 

        call  GFNHIT  (dete_Csys(1:3)//'H',dete_Cdet,Nhits)
        Check dete_onoff>0 & Nhits>0
        ctab='g2t_'//dete_ctab(1:3)//'_hit'//o
        prin3 ctab,dete_Csys,dete_Cdet,nhits
        (' in G2Tmain: found ',3(1x,a),'   Nhits = ',i6)

        if (ctabo .ne. ctab ) nnhits = 0
        ctabo  = ctab
        nnhits = nnhits + Nhits
        i = G2T_NEW_TABLE (ctab, dete_spec, nnhits)
        prin5 i; (' g2tmain ===> tdm table for g2t_hits = ',i6)
        
        Check agfhit0 (dete_Csys,dete_Cdet) == ok
        names(3)=ctab
        i = AMI_CALL ('g2t_get_hits'//o,3,names)
        prin5 i; (' g2tmain ===> ami_call g2t_get_hits  = ',i6)

      enddo
      if (ld>0) i = DUI_CDIR('..'//o)
*
      G2T_MAIN = i
      END


      subroutine G2T_NEW_TABLE(name,spec,L)
      character  o*1

*include "PAM.inc"
#include "g2t_hits.inc"
#include "g2t_svt_hit.inc"
#include "g2t_tpc_hit.inc"
#include "g2t_mwc_hit.inc"
#include "g2t_ctf_hit.inc"
#include "g2t_emc_hit.inc"
#include "g2t_smd_hit.inc"
#include "g2t_eem_hit.inc"
#include "g2t_esm_hit.inc"
#include "g2t_ftp_hit.inc"
#include "g2t_vpd_hit.inc"

      o = char(0)  
      if (spec=='svt') { call TDM_NEW_TABLE (name, g2t_svt_hit_spec//o, L) } 
  elseif (spec=='tpc') { call TDM_NEW_TABLE (name, g2t_tpc_hit_spec//o, L) } 
  elseif (spec=='mwc') { call TDM_NEW_TABLE (name, g2t_mwc_hit_spec//o, L) } 
  elseif (spec=='ctf') { call TDM_NEW_TABLE (name, g2t_ctf_hit_spec//o, L) } 
  elseif (spec=='emc') { call TDM_NEW_TABLE (name, g2t_emc_hit_spec//o, L) } 
  elseif (spec=='smd') { call TDM_NEW_TABLE (name, g2t_smd_hit_spec//o, L) } 
  elseif (spec=='eem') { call TDM_NEW_TABLE (name, g2t_eem_hit_spec//o, L) } 
  elseif (spec=='esm') { call TDM_NEW_TABLE (name, g2t_esm_hit_spec//o, L) } 
  elseif (spec=='ftp') { call TDM_NEW_TABLE (name, g2t_ftp_hit_spec//o, L) } 
  elseif (spec=='vpd') { call TDM_NEW_TABLE (name, g2t_vpd_hit_spec//o, L) }
  else                 { call TDM_NEW_TABLE (name, g2t_hits_spec//o,    L) }

  end


