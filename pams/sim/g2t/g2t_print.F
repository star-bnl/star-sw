      FUNCTION  G2T_PRINT (  p_vert_h,             p_vert ,
     >                       p_track_h,            p_track )
      IMPLICIT  NONE
      INTEGER*4 G2T_PRINT

      INTEGER   It, Iv, Ityp, Npar, Istop, LenT, LenV, LBUF, i,
     >          Nd, Id, NBytPW, LabelV, LabelT, Label1, IHEAD(1000)
      Parameter (Lbuf=200,NBytPW=4)
      INTEGER   NBUF, NtBeam, NtTarg, Ipart, eg_label, Parent, NwHead,
     >          Iv0, Ip0, Mecat, Meca, Ivol, N_parent, numed, Jv, Kv 
      REAL      VMOD, VDIST, Pvert(4), Vert(3), Ubuf(Lbuf), Par(50),
     >          Amass, Tlife, Charge, Tofg, Pt, tet, Eta, rap, 
     >          d,P(4),V(3),am,time,Vcut/0.0010/
      CHARACTER NAPART*24,Cvol*4,Cshap*4
      Integer   NUM(100000),ISTAT,IDPDG,MOTH(2),IDAU(2)

#include       "g2t_print.inc"
#include       "geant321/gcnum.inc"

CC:>-----+---------+---------+---------+---------+---------+---------+---------+
      G2T_PRINT = STAFCV_OK

C: Sanity check of header for table 'p_vert' & 'p_track'
      print *,' p_vert_h.maxlen,  NVERTX = ', p_vert_h.maxlen, NVERTX
      print *,' p_track_h.maxlen, NTRACK = ', p_track_h.maxlen, NTRACK
      print *,' vert,track len = ',p_vert_h.maxlen,p_track_h.maxlen

      Do Iv = 1,min(NVERTX,5)
        Call GFVERT (Iv, Vert, NtBeam, NtTarg, TOFG, UBUF, NBUF)

        print *,'p_vert(iv).id, iv   =', p_vert(iv).id, iv
        print *,'p_vert(iv).eg_label =', p_vert(iv).eg_label
        print *,'p_vert(iv).ge_volume=', p_vert(iv).ge_volume
        print *,'p_vert(iv).ge_tof   =', p_vert(iv).ge_tof

        do i =1,3
        print *,'p_vert(iv).ge_x(i), vert(i) =',p_vert(iv).ge_x(i),vert(i)
        enddo
      EndDo

      END


