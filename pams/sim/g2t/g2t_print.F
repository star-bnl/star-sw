      FUNCTION  G2T_PRINT (  p_vert_h,             p_vert ,
     >                       p_track_h,            p_track )
      IMPLICIT  NONE
      INTEGER   G2T_PRINT, Iv, i, NtBeam, NtTarg, NBUF
      REAL      VERT(3), TOFG, UBUF(100) 

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


