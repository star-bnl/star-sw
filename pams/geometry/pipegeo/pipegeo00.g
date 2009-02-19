******************************************************************************
Module PIPEGEO00 is the SIMPLIFIED geometry  of the STAR beam pipe.
  Created 03/17/08
  Author  Gerrit van Nieuwenhuizen

******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      Content  PIPE,PIPC,PVAC

      Structure PIPV {version,  int pipeConfig}
      Structure PIPG {config,BeInnR,BeOutR,BeLeng}

* -----------------------------------------------------------------------------
      Real Vacuum
* -----------------------------------------------------------------------------
   FILL PIPV             !  Beam pipe version
      version    =  1    ! geometry version
      pipeConfig =  0    ! pipe version
   endFill
* -----------------------------------------------------------------------------
* The thinner new pipe, which will require an exoskeleton
   FILL PIPG             !  Beam Pipe data 02/13/2009: thickness is 30 mill = 762um
      config   =  0      ! both material and geo params
      BeInnR    = 1.9619 ! Berillium section inner radius
      BeOutR    = 2.0381 ! Berillium section outer radius
      BeLeng    = 200.0  ! Berillium section length
   endfill
* -----------------------------------------------------------------------------
*
      Vacuum = 1.0e-5

      USE PIPV
      USE PIPG config=PIPV_pipeConfig;

      write(*,*) '===>GEOINFO/pipegeo00 SIMPLE VERSION of BEAMPIPE!!! - PIPE - created'

      Create   PIPE
      Position PIPE in CAVE
*
* -----------------------------------------------------------------------------
Block PIPE is the STAR beam pipe mother volume
      Material  Air
      Medium    Standard
      Attribute Pipe Seen=0 colo=1

      Shape TUBE Rmin=0           _
                 Rmax=pipg_BeOutR _
                 Dz=pipg_BeLeng/2 

      Create   PIPC
      Position PIPC
      Create   PVAC
      Position PVAC
endblock
* -----------------------------------------------------------------------------
Block PIPC is the Central Beam PIPe Volume
       Material  Berillium
       Attribute Pipc Seen=1 colo=6

       Shape TUBE Rmin=pipg_BeInnR _
                  Rmax=pipg_BeOutR _
                  Dz=pipg_BeLeng/2 
EndBlock
* -----------------------------------------------------------------------------
Block PVAC is the Vacuum Volume of Be section of pipe
       Material  Air
       Attribute Pipc Seen=1 colo=5
       Material  PVacuum   dens=ag_dens*Vacuum,
                           Radl=ag_RadL/Vacuum, AbsL=ag_AbsL/Vacuum
                           
       Shape TUBE Rmin=0           _
                  Rmax=pipg_BeInnR _
                  Dz=pipg_BeLeng/2.0
EndBlock
* -----------------------------------------------------------------------------
      END
