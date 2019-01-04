*******************************************************************************
                Subroutine   TPCELASER
*                                                                             *
* Description: generate correct energy deposition produced by the laser beam  *
*   Regarding the beam intensity and ionization, A. Lebedev wrote in his      *
*   note that the ionization density decreases along the beam according to    *
*   I(l) = I(0)*exp(-l/300.),  l in cm I(0) = 100e-/cm., dE of 1 e- is 26 eV  *
*   PN  21.08.97: anything denser than TPC gas absorbs laserino               *
*   PN  30.03.99: introducing dE dependence on beam intensity (Q measured)    *
*       Normalisation is done on a measured Q peak value for cosmic muons     *
*       Although the beam intensity is sqrt of Q, the ionization is I**2.     *
*******************************************************************************
      implicit none
+CDE,GCMATE,GCTMED,GCKINE,GCTRAK,AGCSTEP.
      character*8  cpart,laserino/'LASERINO'/
      equivalence (cpart,NaPart)
      Real Average
      REAL ANORM/4.2/       " MUON <Q> IN 87 MEASURMENTS "
      REAL PNORM/100./      " Npairs/cm for relativistic muons " 
      Integer Npair,ier
*                                    this happens only with primary laserino   
      Check cpart==laserino
      if (Dens>0.005) Istop=1  
      Check Isvol > 0 & Istak==0
*                                    number of produced pairs follows Poisson
      Average = Pnorm*aStep * GETOT/Anorm * exp(-Sleng/300.)
      Call POISSN(Average,Npair,ier)
      AdEstep  = 26.e-9*Npair
*
      end


*******************************************************************************
                Subroutine   TPADSTEP(j,Hit)
* Description:  Step routine for TPCs, called at the end of the HITS operator *
*    A correction for the track curvature is introduced here. It assumes that
*    hits are (z,y,x)- packed and negative Z are in left-handed coordinates.
*     
*******************************************************************************
*
+CDE,Typing,GCBANK,GCVOLU,GCKINE,GCTRAK,AgCSTEP,GCFLAG.
*/AGCSTEP/: r*7 vect0,vloc0,vloc,xloc, r: Astep,Adestep
*
      Integer   J,MyPad,JMyPad,Ishape,JMyPar,i_flag,Jcenter
      Real      center(3)/0,0,0/,Field(3)/0,0,0/,Hit,Dr,Dt,Vect_middle(6),
                xi,yi,xo,yo,dx,dy,dphi,dtr,Vr,Pt,SIGN,smax/6.0/
      data      i_flag/0/,Jcenter/-1/,Vect_middle/6*0./
      Logical   Valid_hit,First/.true./
      Character Cname*4
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                Entry     TPAISTEP (J,Hit)
                Entry     TPAOSTEP (J,Hit)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      If (First) then
          First = .false.
          Call GUFLD (center,field)
          if (IDEBUG>1) print *,' In TPADSTEP field=',field
      endif
* 
* Extract parameters of the current padraw from GEANT
*
*    This we need to get the actual padraw width from GEANT:
    JMYPAR  = LQ(JGPAR-NLEVEL)         ! pointer to the volume parameters
    dr      = Q(JMYPAR+1)              ! Radial half-size
    dt      = Q(JMYPAR+2)              ! transverse half-size
*
    If (Idebug>0) then
*    this is for control purpouse only - not really needed:
      MyPad   = LVOLUM(NLEVEL)           ! Padrow Volume Pointer (Number)
      JMYPad  = LQ(JVOLUM-MYPad)         ! pointer to the volume bank
      Ishape  = Q(JMyPad+2)              ! GEANT shape code of the PadRaw
      If (Ishape != 1) then
        Call UHTOC(IQ(JMyPad-4),4,CNAME,4) ! should  be TPAD,TPAI or TPAO
        print *,' Awful error in TPADSTEP, valid hit may not be valid !'
        print *,' volume ',cname,' at level ',NLEVEL,' shape=',Ishape,' dr=',dr
        print *,' we are in point ',vect0
      endif 
    endif
*
*    A hit is called Valid when trajectory cross a padrow (almost) completely:
      Valid_hit = abs(vloc0(1)-vloc(1)) .gt. 2*dr-0.1
*
      xi = vloc0(1);      yi = vloc0(2)
      xo = vloc(1);       yo = vloc(2)

      Pt = vloc0(7) * sqrt(vloc0(4)**2+vloc0(5)**2)
      Vr = 1.e9;  if (Pt>0) Vr = 0.0003*Field(3)/Pt

*     more precisely 2*asin(S/2R), but we use a limited linear approximation:
      dphi = min (aStep*Vr,1.0)
      dtr  = dphi/8*charge*SIGN(1.,VECT(3))

*     correction to   HITS TPAD  Z  Y  X  cx cy cz 
*     again precise coefficient is S/R*(1-cos(phi/2), take linear term only:
      dy = dtr*(xi-xo);  hits(2) = min(max(-dt,hits(2)+dy),dt)
      dx = dtr*(yo-yi);  hits(3) = min(max(-dr,hits(3)+dx),dr)

      hit = AdEStep
      end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
* Suppose we have a piece of trajectory from i to o (in xy plane only):
*       _
*      |\       o
*         n   ( / \
*            ( /    \
*            (/       \
*            i---------c
*
* - where c in center of the helix, ('s represent the helix arc. 
* To find its middle point (in direction of n) we introduce vectors:
*         i={xi,yi}
*         o={xo,yo)
*         s={xo-xi,yo-yi}
*         n={yo-yi,xi-xo}/L
* where
*         L=dist(i-o)=|S|
* Then
*         R=p/0.0003B    - R[cm], B[KGs], p[GeV/c]
*         phi=L/R        - linear approximation should be good enouph, 
*                          geant will keep this value < 20 degrees)
*         d=R*(1-cos phi)= ~R*phi^2/8  - displacement 
*         from to middle of the straight line to the middle of the arc
*             
*         m={(xi+xo)/2,(yi+yo)/2} + L/8R*{yo-yi,xi-xo}
*         ============================================
*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
