c $Id: getmass.F,v 1.2 2013/10/22 20:15:35 jwebb Exp $
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#define RANF RANFSTAR
#define ranf RANFSTAR
      real*8 function getmass(ssqrt,type)
c
c     Revision : 1.0 
c
cinput  ssqrt  :  maximum energy available
cinput  type   :  class of resonance defined in getrange
coutput getmass:  mass of the resonance
c
C  DETERMINES MASS OF A non-strange baryon RESONANCE.
c
c     function calls: massdist ranf
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit none
      real*8 ssqrt,mdice,mm,mmax
      integer type
      real*8 ranf
C      include 'comnorm.f'
#include "comnorm.inc"
C      include 'comres.f'
#include "comres.inc"

c...  only n*,d,d* included in n_splint

      mmax=min(mresmax,ssqrt)

c get probability mdice
      call n_splint(x_norm,y_norm,y2a,n,mmax,mdice,type)
      if(mdice.eq.0)write(6,*)'getmass:mdice=',mdice
c for this probability choose mass mm
      call n_splint(y_norm,x_norm,y2b,n,mdice*ranf(0),mm,type)

      if(mm.lt.mresmin) then
         write(6,*)'(W) getmass-error - m, mmin',mm,mresmin
	   mm=mresmin
      else if(mm.gt.mresmax) then 
         write(6,*)'(W) getmass-error - m, ,max',mm,mresmax
	   mm=mresmax
      endif

      getmass=mm

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine norm_init
c
coutput : via common-block comnorm
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
C      include 'comres.f'
#include "comres.inc"
C      include 'comnorm.f'
#include "comnorm.inc"
      real*8 massdist
      real*8 x(n),y(n),y21(n),y22(n)
      integer i,j
c    linear weighting restored
c    in comnorm.f n is set to 400 again
      dx = (mresmax-mresmin)/dble(n-1) ! (n-1)**2 for quad. weight
      do 1 i=0,3
         y_norm(i,1) = 0.0
         y(1) = 0.0
         x_norm(i,1) = mresmin
         x(1) = mresmin
         do 2 j=2,n
            x_norm(i,j) = mresmin+dble(j-1)*dx ! (j-1)**2 for quad. weight
            x(j) = x_norm(i,j)
            y(j) = y(j-1) + (x(j)-x(j-1)) ! valid for both weights
     &                      *(massdist(x(j-1),i)+massdist(x(j),i)
     &                  + 4.0*massdist(0.5*(x(j)+x(j-1)),i))/6.0 
            y_norm(i,j) = y(j)
2        continue
         call spline(x,y,n,massdist(mresmin,i),
     &                    massdist(mresmax,i),y21)
         call spline(y,x,n,1.0/massdist(mresmin,i),
     &                    1.0/massdist(mresmax,i),y22)
         do 3 j=1,n
            y2a(i,j) = y21(j)
            y2b(i,j) = y22(j)
3        continue
1     continue
      return
      end

c Numerical recipies:
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  (modified)
      SUBROUTINE n_splint(xa,ya,y2a,n,x,y,m)
      implicit none
      INTEGER n,m
      real*8 x,y,xa(0:3,n),y2a(0:3,n),ya(0:3,n)
      INTEGER k,khi,klo
      real*8 a,b,h
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(m,k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(m,khi)-xa(m,klo)
      if (h.eq.0.) then
       write(6,*) 'bad xa input in splint'
      end if
      a=(xa(m,khi)-x)/h
      b=(x-xa(m,klo))/h
      y=a*ya(m,klo)+b*ya(m,khi)+((a**3-a)*y2a(m,klo)+
     *   (b**3-b)*y2a(m,khi))*(h**2)/6.
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software.

