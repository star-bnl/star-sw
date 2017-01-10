* $Id: g2t_get_kine.F,v 1.13 2000/01/23 19:04:43 nevski Exp $
*
* $Log: g2t_get_kine.F,v $
* Revision 1.13  2000/01/23 19:04:43  nevski
* unused variables deleted
*
*
* Revision 1.12  2000/01/13 02:57:04  nevski
* particle-track relation corrected
*
CC:>-----+---------+---------+---------+---------+---------+---------+---------+
CC: FILE:        g2t_get_kine.F
CC: AUTHOR:      narod
CC: DESCRIPTION: get KINE and VERT structure from GEANT
CC:
CC: ARGUMENTS:   p_vert/p_vert_h      - pointer/header to vertex table
CC:              p_track/p_track_h    - pointer/header to track table
CC: HISTORY:    
CC:  YF, 12/08/97 - first version
CC:  PN, 12/31/97 - second version
CC:  PN, 05/05/98 - first release version
CC:  PN, 06/24/99 - compress vertex numbering
CC:>-----+---------+---------+---------+---------+---------+---------+---------+
      FUNCTION  G2T_GET_KINE (  p_vert_h,             p_vert ,
     >                          p_track_h,            p_track )
      IMPLICIT  NONE
      INTEGER*4 G2T_GET_KINE

      INTEGER   It, Iv, Ityp, Npar, Istop, LenT, LenV, LBUF,
     >          Nd, Id, NBytPW, LabelV, LabelT, Label1, IHEAD(1000)
      integer   maxnum
      Parameter (Lbuf=200,NBytPW=4,maxnum=1000000)
      INTEGER   NBUF, NtBeam, NtTarg, Ipart, eg_label, Parent, NwHead,
     >          Iv0, Ip0, Mecat, Meca, Ivol, N_parent, numed, Jv, Kv, Md
      REAL      VMOD, VDIST, Pvert(4), Vert(3), Ubuf(Lbuf), Par(50),
     >          Amass, Tlife, Charge, Tofg, Pt, tet, Eta, rap, 
     >          P(4),V(3),am,time,Vcut/0.0010/
      CHARACTER NAPART*24,Cvol*4,Cshap*4
      Integer   NUM(maxnum),ISTAT,IDPDG,MOTH(2),IDAU(2)
      INTEGER             NUMPRIM,NUMFINS
      common /g2tevtcnt/  NUMPRIM,NUMFINS

#include       "g2t_get_kine.inc"
#include       "geant321/gcnum.inc"

CC:>-----+---------+---------+---------+---------+---------+---------+---------+
      G2T_GET_KINE = STAFCV_BAD

C: Sanity check of header for table 'p_vert' & 'p_track'
      IF ( p_vert_h.maxlen  .LT. NVERTX .OR.
     >     p_track_h.maxlen .LT. NTRACK ) then
         print *,' FATAL error in G2T_GET_KINE '
         print *,' vert,track len = ',p_vert_h.maxlen,p_track_h.maxlen
         return
      endif
*
      Call GFHEAD (NwHead, IHead, Nbuf, UBUF)
      If (NwHead.gt.1000 .or. NBUF.gt.Lbuf) go to 901

      LenT          = p_track_h.rbytes/NBytPW
      LenV          = p_vert_h.rbytes /NBytPW
      p_track_h.nok = 0
      p_vert_h.nok  = 0
      LabelV        = 0
      Iv            = 0
      Do Kv = 1,min(NVERTX,maxnum)

        Call VZERO  (p_vert(Kv),LenV)
        Call GFVERT (Kv, Vert, NtBeam, NtTarg, TOFG, UBUF, NBUF)
        If (Nbuf.gt.Lbuf) go to 901
*       
        If (Ntbeam.le.0) then ! this is an event generator vertex

* 24.06.99: filter-out close vertices
          Do Jv = 1,Iv
            IF ( p_vert(jv).eg_label.ne.0       .and. 
     >           VDIST(p_vert(jv).ge_x,Vert,3).le.Vcut) then
               num(min(kv,maxnum)) = Jv
               goto 77
            endif
          enddo  

          LabelV   = LabelV+1
          eg_Label = LabelV
          n_parent = 0
          Parent   = 0
          meca     = 0 
          Numed    = 0 
          Cvol     = '_eg_'
        else                  ! this is a Geant produced vertex
          eg_label = 0
          n_parent = 1
          Parent   = Ntbeam
*                               NtTarg coding from GSSTAK:
*         Mecat = min(LMEC(NMEC),49) + LVOLUM(NLEVEL)*100
*         If (ISTOP.EQ.0) Mecat = Mecat+50 ! flag non-stopped particles
*         If (ISTAK.GT.0) Mecat = -Mecat   ! flag disconnected vertex
*
          Mecat    = abs  (NtTarg)
          meca     = mod  (Mecat,50)
          Istop    = 1-mod(Mecat,100)/50
          if (Istop.eq.0) meca=-meca
          Ivol     = Mecat/100
          Call aGFVOLU (Ivol,Cvol,Cshap,numed,par,npar)
          if (npar.lt.0) print *,' G2T_get_kine error: Vertex ',iv,
     *                           ' refers a non-existing volume',ivol 
        endif
*
        Iv=Iv+1
        Num(kv)=Iv
*
        p_vert(iv).Id           = Iv             ! primary key 
        p_vert(iv).event_p      = Ihead(2)       ! pointer to event
        p_vert(iv).eg_label     = eg_label       ! generator label (0 if GEANT)
        p_vert(iv).eg_tof       = 0              ! vertex production time
        p_vert(iv).eg_proc      = 0              ! event generator mechanism
        Call Vzero(p_vert(iv).eg_x,3)            ! vertex coordinate 
        p_vert(iv).ge_volume    = Cvol           ! GEANT volume name
        p_vert(iv).ge_medium    = Numed          ! GEANT Medium
        p_vert(iv).ge_tof       = Tofg           ! GEANT vertex production time
        p_vert(iv).ge_proc      = meca           ! GEANT mechanism (0 if eg)
        Call Ucopy(Vert,p_vert(iv).ge_x,3)       ! GEANT vertex coordinate
c       p_vert(iv).n_daughter - filled by tracks ! Number of daughter tracks
c       p_vert(iv).daughter_p - filled by tracks ! first daughter in linked list
        p_vert(iv).n_parent     = n_parent       ! number of parent tracks
        p_vert(iv).parent_p     = Parent         ! first parent track
        p_vert(iv).is_itrmd     = 0              ! flags intermediate vertex
        p_vert(iv).next_itrmd_p = 0              ! next intermedate vertex 
        p_vert(iv).next_prim_v_p= 0              ! next primary vertex
        p_vert_h.nok            = iv
 77   EndDo
*
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
      Iv0           = 0
      Ip0           = 0
      LabelT        = 0
      NUMPRIM       = 0
      NUMFINS       = 0
      DO IT = 1,NTRACK

        Call VZERO  (p_track(it),LenT)
        Call GFKINE (It, Vert, Pvert, Ipart,Kv, UBUF, NBUF)
        If (Nbuf.gt.Lbuf) go to 901
*
        IF (Kv.ne.Iv0) THEN
          Call GFVERT (Kv, Vert, NtBeam, NtTarg, TOFG, UBUF, NBUF)
          If (Nbuf.gt.Lbuf) go to 901
          Iv0 = Kv
          Iv  = Num(kv)
        END IF
*        
        If (Ipart.ne.Ip0) then
          Call GFPART (Ipart,NaPart,Ityp,Amass,Charge,Tlife,UBUF,NBUF)
          If (Nbuf.gt.Lbuf) go to 901
          Ip0 = Ipart
        endif
*
        p_track(it).id             = it
        p_track(it).start_vertex_p = Iv
        Call Ucopy (Pvert,p_track(it).p,3)
        p_track(it).ptot           = vmod(Pvert,3)
        p_track(it).e              = Pvert(4)
        p_track(it).ge_pid         = Ipart
        p_track(it).charge         = Charge
        p_track(it).rapidity       = -999.
        p_track(it).pt             = -999.
        p_track(it).eta            = -999.
*
        if (NtBeam .le. 0) then ! this is an event generator particle 
*                                 stable arithmetics requires Pt>0 only
          if (Iv.eq.1) NUMPRIM = NUMPRIM+1
          NUMFINS = NUMFINS + 1
*
          md=0
 61       LabelT  = LabelT + 1 
          call AgNZGETP(1,1,LabelT,ISTAT,IDPDG,P,AM,MOTH,TIME,IDAU,V)
          if (ISTAT.gt.1) go to 61
          Label1  = 99999
          if (ISTAT.eq.1) then
             if (VDIST(Pvert,P,3).gt.0) then
                md = md+1
                go to 61
             endif
             Label1  = LabelT
          endif
          if (md.gt.0 .and. Label1.eq.99999) print *,' g2t_warning -',
     >          ' particle and kine differ: Ikine=',It,',  Eg=',LabelT
*          
          Pt      = VMOD (Pvert,2)
          Eta     = Pvert(3)
          Rap     = Pvert(3)
          If (Pt.gt.Pvert(4)*1.e-3)  then 
            tet   = ATAN2(PT,Pvert(3))      
            Eta   = -LOG (TAN(TET/2))
            Rap   =  LOG ( (Pvert(4)+Pvert(3))/(Pvert(4)-Pvert(3)) )/2
          endif
*
          p_track(it).eg_label   = Label1      ! event generator label
          p_track(it).eg_pid     = Ipart       ! event generator particle ID
          p_track(it).pt         = Pt
          p_track(it).eta        = eta
          p_track(it).rapidity   = rap
*>
          p_vert(iv).eg_label    = Label1      ! generator label (0 if GEANT)
*
        else                ! Ntbeam positive - this is a secondary particle
*                           ! NtTarg coding explained above
          Mecat = abs (NtTarg)
          Istop = 1-mod(Mecat,100)/50

          if      (NtTarg.lt.0) then
          p_track(it).is_shower         = 1    ! this IS a shower track
          else if (Istop .gt.0) then
          p_track(NtBeam).stop_vertex_p = Iv   ! Id of stop vertex of THAT track
          endif
          p_track(it).start_vertex_p    = Iv     ! Id of start vertex of track
          p_track(it).next_parent_p     = NtBeam ! Id of next parent track
        endif 
*
        nd = p_vert(iv).n_daughter
        Id = p_vert(iv).daughter_p 

        p_vert(iv).n_daughter      = nd+1  ! Number of daughter tracks
        p_vert(iv).daughter_p      = it    ! first(last) daughter in linked list
        p_track(it).next_vtx_trk_p = Id    ! Next daughter track of start vertex
*
        p_track_h.nok = it
      end do
*
      G2T_GET_KINE  = STAFCV_OK
      return

 901  print *,' FATAL error in G2T_GET_KINE '
      print *,' UBUF too short, ',NBUF,' needed'
      return
      END

