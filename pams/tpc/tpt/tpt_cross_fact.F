* $Id: tpt_cross_fact.F,v 1.5 1998/09/01 13:32:34 sakrejda Exp $
* $Log: tpt_cross_fact.F,v $
* Revision 1.5  1998/09/01 13:32:34  sakrejda
* restored sector numbering 1-24 in tphit.row
*
* Revision 1.4  1998/08/25 21:46:32  sakrejda
* crossing angle in degrees, STAR convention
*
* Revision 1.3  1998/04/06 19:16:09  sakrejda
* local constants replaced by include, not used variables removed
*
* Revision 1.2  1998/01/27 00:35:44  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_CROSS_FACT(idet,xnew,ynew,xold,yold,cf,theta)
C>------------------------------------------------------------------------
C
C TPT_CROSS_FACT - evaluates crossing angle and weighting factor 
C                  for error
C
C DESCRIPTION:
C The angle is evaluated from a dot prodact of a vector 
C tangent to the track and a vector parallel to the row.
C
C INPUT ARGUMENTS:
C  idet - sector**100 + row
C  xnew - current x
C  ynew - current y
C  xold - previous x
C  yold - previous y
C
C OUTPUT ARGUMENTS:
C  cf   - cos(crossing angle)**2
C  theta- crossing angle
C
C<--------------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE
C______________________________________________________________________
      INTEGER idet
      REAL    xnew,ynew,xold,yold,cf,theta
      real    vlocal(3),vglobal(3)
      integer iret, isect

      INTEGER tgc_sec24_to_sec12, tgc_global_to_local_p

C_____________________________________________________________________

      if(cf.lt.0) then 
         cf=1.0
      else
         iret      = tgc_sec24_to_sec12(idet/100,isect)
         vglobal(1)= xnew-xold
         vglobal(2)= ynew-yold
         vglobal(3)= 0.0
         iret= tgc_global_to_local_p(isect,vglobal,vlocal)
C
         cf = (vlocal(2))**2/(vlocal(2)**2+vlocal(1)**2)
cc      
c        output angle should be in degree
c        ******Qun Li  21/08/98
c        theta = atan(vlocal(1)/vlocal(2))

         theta = atand(vlocal(1)/vlocal(2))
      endif
      tpt_cross_fact = 0
      end










