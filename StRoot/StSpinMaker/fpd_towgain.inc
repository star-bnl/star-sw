*  Definitions for SMD-based pi0 reconstruction.  These parameters
*  set the energy scale of the calorimeter tower pulse height

      real towgainN, towgainF   ! north calorimeter relative gains
      real escal                ! north calorimeter energy scale (GeV/ADC cnt)
      common/towpar/ 
     $     towgainN(12),
     $     towgainF(12),
     $     escal


