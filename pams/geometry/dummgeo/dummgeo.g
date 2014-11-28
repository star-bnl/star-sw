*
* $Id: dummgeo.g,v 1.1 2006/11/22 17:40:27 potekhin Exp $
* $Log: dummgeo.g,v $
* Revision 1.1  2006/11/22 17:40:27  potekhin
* A dummy object to aid in the material balance
* study.
*
* 
******************************************************************************
Module DUMMGEO is a dummy object used to simulate material balance effects
  Created  11/22/06
  Author   Maxim Potekhin
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      Content  DUMM
*
      Structure DUMG {version, Length, Rin, Thk}
*
* -----------------------------------------------------------------------------
*
   Fill DUMG                   ! Dummy object data
      version    =  2          ! Version
      Length     =  48.0       ! Total Length
      Rin        =   8.5       ! Inner Radius
      Thk        =   0.1       ! Thickness
   EndFill

******************************************************
      USE      DUMG
*
      write(*,*) 'This is the dummy cylinder'
      Create   DUMM
      Position DUMM in CAVE   Konly='ONLY'

* -----------------------------------------------------------------------------
Block DUMM is the dummy object
      Material Aluminium
      Attribute DUMM  Seen=1  colo=6

      Shape TUBE Rmin=DUMG_Rin Rmax=DUMG_Rin+DUMG_Thk Dz=DUMG_Length/2.0

endblock
* -----------------------------------------------------------------------------
      END

