

      subroutine rdarrynlo(x,q2,aux,iflag)
      implicit real*8(a-h,o-z)
      implicit integer(i-n)
      dimension aux(6)
      common/pdist/arraya(151,20,6)
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      
      nx=151
      ndata=nx-1
      nq2pts=20
      nq2inv=nq2pts-1
      q2sta=1.d0
      q2fin=1.d6
      ymin=5.d0
      xmin=10.d0**(-ymin)
      xmax=1.d0

      if (q2.lt.q2sta) then
         q2=q2sta
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : Q^2 set to minimal value !',Q2
         ENDIF   
      endif
      if (q2.gt.q2fin) then
         q2=q2fin
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : Q^2 set to maximal value !',Q2
         ENDIF
      endif
      if (x.lt.xmin) then
         x=xmin
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : X set to minimal value !',X
         ENDIF   
      endif
      if (x.gt.xmax) then
         x=xmax
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : X set to maximal value !',X
         ENDIF         
      endif

      y=dlog10(x)
      ram=(y+ymin)*ndata/ymin+1.d0
      iram=int(ram)
      fraci=ram-dble(iram)
      ram=dlog(q2/q2sta)*nq2inv/dlog(q2fin/q2sta)+1.d0
      jram=int(ram)
      fracj=ram-dble(jram)
      
      do i=1,6
         aux(i)=(arraya(iram,jram,i)*(1.d0-fraci)
     .        +arraya(iram+1,jram,i)*fraci)*(1.d0-fracj)+
     .        (arraya(iram,jram+1,i)*(1.d0-fraci)
     .        +arraya(iram+1,jram+1,i)*fraci)*fracj
      enddo

      return
      end
