       subroutine agfpartic(link,len)
       implicit   none
       integer    link,len,lk,LGENE,LGENP,ND
+CDE,GCBANK,SCLINK.
       link=0
       len =0
       if (LkEvnt.le.0)       return
       lk=Lkevnt
       if (IQ(Lk-2).lt.2)     return
       LGENE = LQ(Lk-2)
       if (LGENE.le.0)        return
       if (IQ(LGENE-1).lt.12) return
       if (IQ(LGENE-2).lt.1)  return
       LGENP = LQ(LGENE-1)
       if (LGENP.le.0) return
       ND    = IQ(LGENP-1)
       Len   = ND/15
       Link  = LGENP
       end
