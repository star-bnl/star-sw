* auxCD.F
* auxillary functions used by the three- and four-point integrals
* these functions are adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* they are used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"


	double complex function ln(x, isig)
	implicit none
	double precision x, isig

#include "ff.h"

	if( x .gt. 0 ) then
	  ln = log(x)
	else
	  ln = log(-x) +
     &      dcmplx(0D0, sign(pi, isig))
	endif
	end

************************************************************************

	double complex function cln(z, isig)
	implicit none
	double complex z
	double precision isig

#include "ff.h"

	if( dimag(z) .eq. 0 .and. dble(z) .le. 0 ) then
#ifdef WARNINGS
	  if(isig .eq. 0) print *, "cln: argument on cut"
#endif
	  cln = log(-z) +
     &      dcmplx(0D0, sign(pi, isig))
	else
	  cln = log(z)
	endif
	end

************************************************************************

	double complex function spence(z, isig)
	implicit none
	double complex z
	double precision isig

#include "ff.h"

	double complex z1
	double precision az1

	double complex li2series, cln
	external li2series, cln

	z1 = 1 - z
	az1 = abs(z1)

#ifdef WARNINGS
	if( isig .eq. 0 .and. 
     &    dimag(z) .eq. 0 .and. abs(dble(z1)) .lt. CALACC )
     &    print *, "spence: argument on cut"
#endif

	if( az1 .lt. 1D-15 ) then
	  spence = pi6
	else if( dble(z) .lt. .5D0 ) then
	  if( abs(z) .lt. 1 ) then
	    spence = li2series(z, isig)
	  else
	    spence = -pi6 -
     &        .5D0*cln(-z, -isig)**2 - li2series(1/z, -isig)
	  endif
	else
	  if( az1 .lt. 1 ) then
	    spence = pi6 -
     &        cln(z, isig)*cln(z1, -isig) - li2series(z1, -isig)
	  else
	    spence = 2*pi6 +
     &        .5D0*cln(-z1, -isig)**2 - cln(z, isig)*cln(z1, -isig) +
     &        li2series(1/z1, isig)
	  endif
	endif
	end

************************************************************************

	double complex function li2series(z, isig)
	implicit none
	double complex z
	double precision isig

	double complex xm, x2, new
	integer j

	double complex cln
	external cln

* these are the even-n Bernoulli numbers, already divided by (n + 1)!
* as in Table[BernoulliB[n]/(n + 1)!, {n, 2, 50, 2}]
	double precision b(25)
	data b /
     &    0.02777777777777777777777777777777777777777778774D0, 
     &    -0.000277777777777777777777777777777777777777777778D0, 
     &    4.72411186696900982615268329554043839758125472D-6, 
     &    -9.18577307466196355085243974132863021751910641D-8, 
     &    1.89788699889709990720091730192740293750394761D-9, 
     &    -4.06476164514422552680590938629196667454705711D-11, 
     &    8.92169102045645255521798731675274885151428361D-13, 
     &    -1.993929586072107568723644347793789705630694749D-14, 
     &    4.51898002961991819165047655285559322839681901D-16, 
     &    -1.035651761218124701448341154221865666596091238D-17, 
     &    2.39521862102618674574028374300098038167894899D-19, 
     &    -5.58178587432500933628307450562541990556705462D-21, 
     &    1.309150755418321285812307399186592301749849833D-22, 
     &    -3.087419802426740293242279764866462431595565203D-24, 
     &    7.31597565270220342035790560925214859103339899D-26, 
     &    -1.740845657234000740989055147759702545340841422D-27, 
     &    4.15763564461389971961789962077522667348825413D-29, 
     &    -9.96214848828462210319400670245583884985485196D-31, 
     &    2.394034424896165300521167987893749562934279156D-32, 
     &    -5.76834735536739008429179316187765424407233225D-34, 
     &    1.393179479647007977827886603911548331732410612D-35, 
     &    -3.372121965485089470468473635254930958979742891D-37, 
     &    8.17820877756210262176477721487283426787618937D-39, 
     &    -1.987010831152385925564820669234786567541858996D-40, 
     &    4.83577851804055089628705937311537820769430091D-42 /

	xm = -cln(1 - z, -isig)
	x2 = xm**2
	li2series = xm - x2/4D0
	do j = 1, 25
	  xm = xm*x2
	  new = li2series + xm*b(j)
	  if( new .eq. li2series ) return
	  li2series = new
	enddo
#ifdef WARNINGS
	print *, "li2series: bad convergence"
#endif
	end

************************************************************************

	integer function eta(c1, c2, im1x, im2x, im12x)
	implicit none
	double complex c1, c2
	double precision im1x, im2x, im12x

	double precision im1, im2, im12

	im1 = dimag(c1)
	if( im1 .eq. 0 ) im1 = im1x
	im2 = dimag(c2)
	if( im2 .eq. 0 ) im2 = im2x
	im12 = dimag(c1*c2)
	if( im12 .eq. 0 ) im12 = im12x

	if( im1 .lt. 0 .and. im2 .lt. 0 .and. im12 .gt. 0 ) then
	  eta = 1
	else
     &  if( im1 .gt. 0 .and. im2 .gt. 0 .and. im12 .lt. 0 ) then
	  eta = -1
	else
	  eta = 0
#ifdef WARNINGS
	  if( .not. (im2 .eq. 0 .and. dble(c2) .gt. 0 .or.
     &               im1 .eq. 0 .and. dble(c1) .gt. 0) .and.
     &        (im1 .eq. 0 .and. dble(c1) .lt. 0 .or.
     &         im2 .eq. 0 .and. dble(c2) .lt. 0 .or.
     &         im12 .eq. 0 .and. dble(c1*c2) .lt. 0) )
     &      print *, "eta not defined"
#endif
	endif
	end

