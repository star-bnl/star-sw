      real*8 function determinant(a11,a12,a13,a21,a22,a23,a31,a32,a33)  
      implicit none

C     SYNOPSIS:
C     calculates the determinant of a 3x3 matrix
C     
C     DESCRIPTION:
C     Calculates the determinant (non-iteratively) of a 3x3 matrix
C
C     determinant = | a11  a12  a13 |
C                   | a21  a22  a23 |
C                   | a31  a32  a33 |
C
C     AUTHOR:
C     C. Pruneau,  Wayne State University
C     
C     DATE:
C     18-OCT-1993
C     
C     ARGUMENTS:
C     aij   : elements of the matrix
C      
      real*8 a11,a12,a13,a21,a22,a23,a31,a32,a33
      
      determinant=
     &     a11*(a22*a33-a23*a32) - 
     &     a12*(a21*a33-a23*a31) + 
     &     a13*(a21*a32-a22*a31)

      return
      end

c     --------------------------------------------------------------------
      subroutine linfit(x,y,dx,dy,n,a,da,chi2)
      implicit none

C     SYNOPSIS:
C     Linear fitting subroutine
C     
C     DESCRIPTION:
C     Linear fitting with x as the independant variable and y the dependant.
C     An error on both x and y is allowed altough currently only the y-error
C     is considered in the fitting. The error is also 
C     assumed to be statistical only. Using both the following forms:
C
C     y = a(1) + a(2)*x
C     
C     x = b(1) + b(2)*y
C
C     with the understanding that only  the first one will be used if
C     the determinant "d" is greater than zero in that case. If the
C     second form is used, the coefficients b will be converted to a.
C
C     USAGE:  
C     Points with negative errors are not included in the fit.
C
C     AUTHOR:
C     C. Pruneau,  Wayne State University
C     
C     DATE:
C     18-OCT-1993
C     
C     ARGUMENTS:
C     x     : independant variable
C     y     : dependant
C     dx    : estimated error on x
C     dy    : estimated error on y
C     n     : number of points to consider
C     a     : coefficients
C     da    : estimated error on coefficients a
C     chi2  : calculated chi-square (wkw: and reduced)
C
C     NOTE:
C     Errors: da(1) : sigma(a(1))*2
C             da(2) : sigma(a(2))*2
C             da(3) : sigma(a(1),a(2))
C
c     argument declarations
c     ---------------------
      integer n
      real    x(n), y(n), dx(n), dy(n), a(2), da(3), chi2

c     local declarations
c     ------------------
      real*8  s, sx, sxx, sxy, syy, sy, w, d1, d2, chi
      real*8  b(2), db(3)
      integer i
      real    infinity
      real    dmin
c wkw      parameter (dmin=1.e-12)
      parameter (dmin=1.e-20)
      parameter (infinity=1.e16)

c     executable
c     ----------
      s    = 0.
      sx   = 0.
      sxx  = 0.
      sxy  = 0.
      syy  = 0.
      sy   = 0.

c     loop over all relevant points

      do i = 1, n
         if (dy(i).gt.0.) then
            w   = 1./dy(i)**2
            s   = s + w
            sx  = sx  + w*x(i)
            sxx = sxx + w*x(i)*x(i)
            sxy = sxy + w*x(i)*y(i)
            syy = syy + w*y(i)*y(i)
            sy  = sy  + w*y(i)
         end if
      end do

c     calculate coefficients

      d1  = s*sxx - sx*sx
      d2  = s*syy - sy*sy

      if (d2.gt.d1) then		! use second form
         if (d2.le.dmin) then         ! second form also fails
            print*,'Failed d2 test in linfit' ! wkw: temporary comment
            chi2  = infinity
            a(1)  = infinity
            a(2)  = infinity
            da(1) = infinity
            da(2) = infinity
            da(3) = infinity
         else                           ! second branch

c     calculate b and errors

            b(1)  = (syy*sx-sy*sxy)/d2
            b(2)  = (s*sxy-sx*sy)/d2
            db(1) = syy/d2
            db(2) = s/d2
            db(3) = -sy/d2              ! wrong - to be corrected

c     calculate chi-square
            
            chi = 0.
            do i = 1, n
               if (dy(i).gt.0.) then
                  chi = chi + ((x(i)-b(1)-b(2)*y(i))/dx(i))**2
               end if
            end do

c     calculate a(i) and errors

            a(1)  = -b(1)/b(2)
            a(2)  = 1./b(2)
            da(1) = abs(b(1)*db(2)/b(2)**2 - db(1)/b(2))
            da(2) = abs(a(2)*db(2)/b(2))
            da(3) = (b(2)*db(3)-b(1)*db(2))/b(2)**4

c     store chi2 for output

            chi2 = chi/(float(n)-2.) ! wkw: reduced
         end if
      else                             ! normal branch 

c     calculate a and errors

         a(1)  = (sxx*sy-sx*sxy)/d1
         a(2)  = (s*sxy-sx*sy)/d1
         da(1) = sxx/d1
         da(2) = s/d1
         da(3) = -sx/d1

c     calculate chi-square

         chi = 0.
         do i = 1, n
            if (dy(i).gt.0.) then
               chi = chi + ((y(i)-a(1)-a(2)*x(i))/dy(i))**2
            end if
         end do
         
         chi2 = chi/(float(n)-2.) ! wkw: reduced
      end if

      return
      end

c     --------------------------------------------------------------------
      subroutine linfitr(x,y,dx,dy,n,a,da,chi2,res)
      implicit none

C     SYNOPSIS:
C     Linear fitting subroutine
C     
C     DESCRIPTION:
C     Linear fitting with x as the independant variable and y the dependant.
C     An error on both x and y is allowed altough currently only the y-error
C     is considered in the fitting. The error is also 
C     assumed to be statistical only. Using both the following forms:
C
C     y = a(1) + a(2)*x
C     
C     x = b(1) + b(2)*y
C
C     with the understanding that only  the first one will be used if
C     the determinant "d" is greater than zero in that case. If the
C     second form is used, the coefficients b will be converted to a.
C
C     USAGE:  
C     Points with negative errors are not included in the fit.
C
C     AUTHOR:
C     C. Pruneau,  Wayne State University
C     
C     DATE:
C     18-OCT-1993
C
C     Modified by wkw to output residues June-1994
C     
C     ARGUMENTS:
C     x     : independant variable
C     y     : dependant
C     dx    : estimated error on x
C     dy    : estimated error on y
C     n     : number of points to consider
C     a     : coefficients
C     da    : estimated error on coefficients a
C     chi2  : calculated chi-square
C
C     NOTE:
C     Errors: da(1) : sigma(a(1))*2
C             da(2) : sigma(a(2))*2
C             da(3) : sigma(a(1),a(2))
C
c     argument declarations
c     ---------------------
      integer n
      real    x(n), y(n), dx(n), dy(n), a(2), da(3), chi2
      real    res(n) ! wkw

c     local declarations
c     ------------------
      real*8  s, sx, sxx, sxy, syy, sy, w, d1, d2, chi
      real*8  b(2), db(3)
      integer i
      real    infinity
      real    dmin
c wkw      parameter (dmin=1.e-12)
      parameter (dmin=1.e-20)
      parameter (infinity=1.e16)

c     executable
c     ----------
      s    = 0.
      sx   = 0.
      sxx  = 0.
      sxy  = 0.
      syy  = 0.
      sy   = 0.

c     loop over all relevant points

      do i = 1, n
         if (dy(i).gt.0.) then
            w   = 1./dy(i)**2
            s   = s + w
            sx  = sx  + w*x(i)
            sxx = sxx + w*x(i)*x(i)
            sxy = sxy + w*x(i)*y(i)
            syy = syy + w*y(i)*y(i)
            sy  = sy  + w*y(i)
         end if
      end do

c     calculate coefficients

      d1  = s*sxx - sx*sx
      d2  = s*syy - sy*sy

      if (d2.gt.d1) then		! use second form
         if (d2.le.dmin) then         ! second form also fails
            print*,'Failed d2 test in linfit' ! wkw: temporary comment
            chi2  = infinity
            a(1)  = infinity
            a(2)  = infinity
            da(1) = infinity
            da(2) = infinity
            da(3) = infinity
            do i=1,n
               res(n) = infinity
            enddo
         else                           ! second branch

c     calculate b and errors

            b(1)  = (syy*sx-sy*sxy)/d2
            b(2)  = (s*sxy-sx*sy)/d2
            db(1) = syy/d2
            db(2) = s/d2
            db(3) = -sy/d2              ! wrong - to be corrected

c     calculate chi-square
            
            chi = 0.
            do i = 1, n
               if (dy(i).gt.0.) then
                  chi = chi + ((x(i)-b(1)-b(2)*y(i))/dx(i))**2
                  res(i) = x(i)-b(1)-b(2)*y(i)
               end if
            end do

c     calculate a(i) and errors

            a(1)  = -b(1)/b(2)
            a(2)  = 1./b(2)
            da(1) = abs(b(1)*db(2)/b(2)**2 - db(1)/b(2))
            da(2) = abs(a(2)*db(2)/b(2))
            da(3) = (b(2)*db(3)-b(1)*db(2))/b(2)**4

c     store chi2 for output

            chi2 = chi/(float(n)-2.) ! wkw: reduced
         end if
      else                             ! normal branch 

c     calculate a and errors

         a(1)  = (sxx*sy-sx*sxy)/d1
         a(2)  = (s*sxy-sx*sy)/d1
         da(1) = sxx/d1
         da(2) = s/d1
         da(3) = -sx/d1

c     calculate chi-square

         chi = 0.
         do i = 1, n
            if (dy(i).gt.0.) then
               chi = chi + ((y(i)-a(1)-a(2)*x(i))/dy(i))**2
               res(i) = y(i)-a(1)-a(2)*x(i)
            end if
         end do
         
         chi2 = chi/(float(n)-2.) ! wkw: reduced
      end if

      return
      end

c     -------------------------------------------------------------------
      subroutine parabolfit(x,y,dx,dy,n,a,da,chi2)
      implicit none

c     SYNOPSIS:
C     parabolfit - parabol fitting subroutine
C     
C     DESCRIPTION:
C     Parabol fitting with x as the independant variable and y the 
c     dependant.  An error on both x and y is allowed altough currently 
c     only the y-error is considered in the fitting. The error is also 
C     assumed to be statistical only. Eventually using both the following 
c     forms:
C
C     y = a(1) + a(2)*x + a(3)*x**2
C     
C     x = b(1) + b(2)*y + b(3)*x**2
C
C     with the understanding that only  the first one will be used if
C     the determinant "d" is greater than zero in that case. If the second 
C     form is used, the coefficients b will be converted to a.
C
C     USAGE:  
C     Points with negative errors are not included in the fit.
C
C     AUTHOR:
C     C. Pruneau,  Wayne State University
C     
C     DATE:
C     24-Nov-1993
C     
C     ARGUMENTS:
C     x     : independant variable
C     y     : dependant
C     dx    : estimated error on x
C     dy    : estimated error on y
C     n     : number of points to consider
C     a     : coefficients
C     da    : estimated error on coefficients a
C     chi2  : calculated chi-square



c     argument declarations
c     =====================
      integer n, m
      real    x(n), y(n), dx(n), dy(n), a(3), da(6), chi2

c     local declarations
c     ==================
      real*8  s, sx, sx2, sx3, sx4, sx2y, sxy2, sxy, sy, sy2,sy3, sy4
      real*8  w, d, chi
      integer i
      real*8  determinant
      real    infinity
      real    dmin
      parameter (dmin=1.e-10)
      parameter (infinity=1.e12)


c     format
c     ======
 1001 format(1x,' <F>-parabolfit: insuficient number of points:',i5)
 1002 format(1x,' <F>-parabolfit: insuficient number of points ',
     &     'with dy>0:',i5)


c     executable
c     ==========
      if (n.lt.4) then
         chi2  = infinity
         a(1)  = infinity
         a(2)  = infinity
         a(3)  = infinity
         da(1) = infinity
         da(2) = infinity
         da(3) = infinity
         da(4) = infinity
         da(5) = infinity
         da(6) = infinity
         write(6,1001) n
         return
      end if
      s    = 0.
      sx   = 0.
      sx2  = 0.
      sx3  = 0.
      sx4  = 0.
      sxy  = 0.
      sx2y = 0.
      sxy2 = 0.
      sy   = 0.
      sy2  = 0.
      sy3  = 0.
      sy4  = 0.

c     loop over all relevant points

      m = 0
      do i = 1, n
         if (dy(i).gt.0.) then
            m = m + 1
            w   = 1./dy(i)**2
            s   = s + w
            sx  = sx  + w*x(i)
            sx2 = sx2 + w*x(i)*x(i)
            sx3 = sx3 + w*x(i)*x(i)*x(i)
            sx4 = sx4 + w*x(i)*x(i)*x(i)*x(i)
            sxy = sxy + w*x(i)*y(i)
            sx2y= sx2y+ w*x(i)*x(i)*y(i)
            sxy2= sxy2+ w*x(i)*y(i)*y(i)
            sy  = sy  + w*y(i)
            sy2 = sy2 + w*y(i)*y(i)
            sy3 = sy3 + w*y(i)*y(i)*y(i)
            sy4 = sy4 + w*y(i)*y(i)*y(i)*y(i)
         end if
      end do
      if (m.lt.4) then
         chi2  = infinity
         a(1)  = infinity
         a(2)  = infinity
         a(3)  = infinity
         da(1) = infinity
         da(2) = infinity
         da(3) = infinity
         da(4) = infinity
         da(5) = infinity
         da(6) = infinity
         write(6,1001) m
         return
      end if

      
c     calculate coefficients

      d  = determinant( s,   sx,  sx2, 
     &                  sx,  sx2, sx3,
     &                  sx2, sx3, sx4)


c     calculate a 

      a(1) = determinant( sy,   sx,  sx2, 
     &                     sxy,  sx2, sx3,
     &                    sx2y, sx3, sx4)/d   ! intercept
      a(2) = determinant( s,   sy,   sx2, 
     &                    sx,  sxy,  sx3,
     &                    sx2, sx2y, sx4)/d   ! slope
      a(3) = determinant( s,   sx,   sy, 
     &                    sx,  sx2,  sxy,
     &                    sx2, sx3,  sx2y)/d   ! slope

c     calculate errors


c     calculate chi-square

      chi = 0.
      do i = 1, n
         if (dy(i).gt.0.) then
            chi = chi + ((y(i)-a(1)-a(2)*x(i)-a(3)*x(i)*x(i))/dy(i))**2
         end if
      end do
      
      chi2 = chi
      
      return
      end

