CC:>--------------------------------------------------------------------
CC: FILE:       g2t_get_kine.F
CC: ROUTINE:    G2T_GET_KINE
CC: AUTHOR:     narod
CC:
CC: DESCRIPTION: get KINE and VERT structure from GEANT
CC: ARGUMENTS:
CC:         OUT:
CC:           p_vert/p_vert_h     - pointer/header to vertex table
CC:          p_track/p_track_h    - pointer/header to track table
CC: RETURNS:    STAF Condition Value
CC: HISTORY:    
CC:  YF, 12/08/97 - first version
CC:  PN, 12/11/97 - second version
CC:<--------------------------------------------------------------------

      FUNCTION  G2T_GET_KINE (
     >             p_vert_h,             p_vert ,
     >             p_track_h,            p_track )
      IMPLICIT  NONE
      INTEGER*4 G2T_GET_KINE

#include       "g2t_get_kine.inc"
#include       "geant321/gcnum.inc"

      INTEGER      It, Iv, Ivert, Ityp, idevt, Npar, Istop, LenT,LenV
      INTEGER      NBUF, NtBeam, NtTarg, Ipart, Label, eg_label, Parent,
     >             Iv0, Ip0, Nt, Mecat, Meca,Ivol,N_parent,numed
                   
      REAL         Pvert(4),Vert(3),VMOD,UBUF(100),
     >             Amass,Tlife,Charge,Tofg,ETA,
     >             Pt,tet,rap,par(50)
      CHARACTER    NAPART*24,Cvol*4,Cshap*4

C: Sanity check of header for table 'p_vert' & 'p_track'
      G2T_GET_KINE = STAFCV_BAD
      IF ( p_vert_h.maxlen  .LE. 0 .OR.
     >     p_track_h.maxlen .LE. 0 )    RETURN
*
      LenT          = p_track_h.rbytes/4
      LenV          = p_vert_h.rbytes/4
      p_vert_h.nok  = 0
      p_track_h.nok = 0
      Iv0           = 0
      Ip0           = 0
      p_track_h.nok = 0
      p_vert_h.nok  = 0
      Label         = 0

      Do Iv = 1,NVERTX
        Call GFVERT (Iv, Vert, NtBeam, NtTarg, TOFG, UBUF, NBUF)
        If (Nbuf.gt.100) go to 901
        Call VZERO (p_vert(iv).ge_volume,LenV)
        Cvol='_eg_'
*       
        If (Ntbeam.le.0) then ! this is an event generator vertex
          label    = label+1
          eg_label = label
          n_parent = 0
          Parent   = 0
        else                  ! this is a Geant produced vertex
          eg_label = 0
          n_parent = 1
          Parent   = abs (Ntbeam)
*                                from GSSTAK:
*         Mecat = min(LMEC(NMEC),49) + LVOLUM(NLEVEL)*100
*         If (ISTOP.EQ.0) Mecat = Mecat+50 ! flag non-stopped particles
*         If (ISTAK.GT.0) Mecat = -Mecat   ! flag disconnected vertex
*
          Mecat = abs (NtTarg)
          meca  = mod (Mecat,50)
          Istop = mod (Mecat,100)/50
          Ivol  = Mecat/100
          Call aGFVOLU (Cvol,Cshap,numed,par,npar,Ivol)
        endif
*
        p_vert_h.nok            = Iv
        p_vert(iv).Id           = Iv                ! primary key 
        p_vert(iv).event_p      = Idevt             ! pointer to event
        p_vert(iv).eg_label     = eg_label          ! generator label (0 if GEANT)
        p_vert(iv).eg_tof       = 0                 ! vertex production time
        p_vert(iv).eg_proc      = 0                 ! event generator mechanism
        Call Vzero(p_vert(iv).eg_x,3)               ! vertex coordinate 
        p_vert(iv).ge_volume    = Cvol              ! GEANT volume name
        p_vert(iv).ge_medium    = Numed             ! GEANT Medium
        p_vert(iv).ge_tof       = Tofg              ! GEANT vertex production time
        p_vert(iv).ge_proc      = meca              ! GEANT mechanism (0 if eg)
        Call Ucopy(Vert,p_vert(iv).ge_x,3)          ! GEANT vertex coordinate
c       p_vert(iv).n_daughter   = Q(LQ(JVERTX-iv)+7)! Number of daughter tracks
c       p_vert(iv).daughter_p   = Q(LQ(JVERTX-iv)+8)! first daughter in linked list
        p_vert(iv).n_parent     = n_parent          ! number of parent tracks
        p_vert(iv).parent_p     = Parent            ! first parent track
        p_vert(iv).is_itrmd     = 0                 ! flags intermediate vertex
        p_vert(iv).next_itrmd_p = 0                 ! next intermedate vertex 
        p_vert(iv).next_prim_v_p= 0                 ! next primary vertex
*      
      EndDo
*
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
      Label         = 0
      DO IT = 1,NTRACK
        Call VZERO (p_track(it).eg_label,LenT)

        Call GFKINE (It, Vert, Pvert, Ipart,Ivert, UBUF, NBUF)
        If (Nbuf.gt.100) go to 901
*
        IF (IVERT.ne.Iv0) THEN
          Iv0 = IVERT
          Call GFVERT (Iv0, Vert, NtBeam, NtTarg, TOFG, UBUF, NBUF)
          If (Nbuf.gt.100) go to 901
        END IF
*        
        If (Ipart.ne.Ip0) then
          Ip0 = Ipart
          Call GFPART (Ip0,NaPart,Ityp,Amass,Charge,Tlife,UBUF,NBUF)
          If (Nbuf.gt.100) go to 901
        endif
*
        nt                    = it
        p_track_h.nok         = it
        p_track(nt).id        = it
        p_track(nt).start_vertex_p = Ivert
        p_track(nt).eg_label  =  0
        p_track(nt).is_shower =  0
        Call Ucopy (Pvert,p_track(nt).p,3)
        p_track(nt).ptot      = vmod(Pvert,3)
        p_track(nt).e         = Pvert(4)
        p_track(nt).ge_pid    = Ipart
        p_track(nt).charge    = Charge
        p_track(nt).rapidity  = -999.
        p_track(nt).pt        = -999.
        p_track(nt).eta       = -999.

        if (Ntbeam .le. 0) then  ! this is an event generator particle        
*                                  stable arithmetics requires Pt>0 only
          label = label + 1 
          Pt    = VMOD (Pvert,2)
          If (Pt.gt.0) then 
            tet = ATAN2(PT,Pvert(3))      
            Eta = -LOG (TAN(TET/2))
            Rap =  LOG ((Pvert(4) + Pvert(3))/(Pvert(4) - Pvert(3)))/2
*
            p_track(nt).eg_label       = label
            p_track(nt).pt             = Pt
            p_track(nt).eta            = eta
            p_track(nt).rapidity       = rap
          end if
        else                     ! this is a secondary particle
*                                from GSSTAK:
*           Mecat = min(LMEC(NMEC),49) + LVOLUM(NLEVEL)*100
*           If (ISTOP.EQ.0) Mecat = Mecat+50 ! flag non-stopped particles
*           If (ISTAK.GT.0) Mecat = -Mecat   ! flag disconnected vertex
*
            Mecat = abs (NtTarg)
            meca  = mod (Mecat,50)
            Ivol  = NtTarg/100
*
            if (NtTarg.lt.0) 
     >      p_track(nt).is_shower      = 1
            p_track(nt).start_vertex_p = Ivert
        endif
*
*       all done, increase table count
        p_track_h.nok = nt
*
      end do
*
      G2T_GET_KINE=STAFCV_OK
      return

 901  print *,' UBUF too short, ',NBUF,' needed'
      print *,' FATAL error in G2T_GET_KINE '

      END

* =========================================================================

      SUBROUTINE  aGFVOLU (Cvol,Cshap,numed,par,npar,Ivol)
      implicit  none
#include       "geant321/gcbank.inc"
      integer   Ivol,numed,npar,Ishap
      character Cvol*4,Cshap*4
      real      par(*)
      CHARACTER*4 CSHAPES(30)
      SAVE CSHAPES
      DATA CSHAPES/'BOX ','TRD1','TRD2','TRAP','TUBE','TUBS','CONE',
     +             'CONS','SPHE','PARA','PGON','PCON','ELTU','HYPE',
     +               13 * '    ', 'GTRA','CTUB','    '/

*         make GFVOLU call... ? => CVOL,CSHAP
*         Call GFVOLU (Ivol,CVOL,CSHAP)
          Call UHTOC   (IQ(JVOLUM+IVOL),4,Cvol,4)
          Ishap     = Q(LQ(JVOLUM-IVOL)+2)
          Cshap     = Cshapes(Ishap)
          Numed     = Q(LQ(JVOLUM-IVOL)+4)
          Npar      = Q(LQ(JVOLUM-IVOL)+5)
c         Numat     = Q(LQ(JTMED-Numed)+6)
          Call Ucopy (Q(LQ(JVOLUM-IVOL)+7),par,min(50,Npar) )
       end

