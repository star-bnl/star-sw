      Function quickid(rig_in, dedx_in)

CCC
CCC   A function to provide temporary PID for HIJING MDC1 files.
CCC
CCC   Author: Richard Witt, Kent State University
CCC
CCC   Last Modification: 7/22/99
CCC

         Implicit none

         Integer e_minus_type     !! Variables to hold the GEANT ID for
         Integer pi_minus_type    !! the various particle types.
         Integer k_minus_type
         Integer e_plus_type
         Integer pi_plus_type
         Integer k_plus_type
         Integer proton_type
         Integer anti_proton_type
         Integer heavy_type
         Integer anti_heavy_type

         Integer  quickid
         Real     rig_in
         Real     dedx_in
         Real     slope
         Real     intrcpt1,intrcpt2,intrcpt3,intrcpt4,intrcpt5
         Real     eupr,elwr
         Real     line1,line2,line3,line4,line5

         Parameter (e_minus_type = 3)
         Parameter (pi_minus_type = 9)
         Parameter (k_minus_type = 12)
         Parameter (e_plus_type = 2)
         Parameter (pi_plus_type = 8)
         Parameter (k_plus_type = 11)
         Parameter (proton_type = 14)
         Parameter (anti_proton_type = 15)
         Parameter (heavy_type = 600)
         Parameter (anti_heavy_type = 700)

         Parameter(slope = -1.60215)       ! Parameters gotten from a set of
         Parameter(intrcpt1 = 3.58674e-8)  ! reconstructed HIJING events.
         Parameter(intrcpt2 = 1.37959e-7)
         Parameter(intrcpt3 = 4.34418e-7)
         Parameter(intrcpt4 = 1.02969e-6)
         Parameter(intrcpt5 = 3.01950e-6)
         Parameter(eupr = 2.66061e-6)
         Parameter(elwr = 1.80413e-6)

         line1 = intrcpt1 * (abs(rig_in)**slope)
         line2 = intrcpt2 * (abs(rig_in)**slope)
         line3 = intrcpt3 * (abs(rig_in)**slope)
         line4 = intrcpt4 * (abs(rig_in)**slope)
         line5 = intrcpt5 * (abs(rig_in)**slope)

         If (dedx_in .lt. line1) then
            If (rig_in .gt. 0.0) then
               quickid = e_plus_type
               Return
            else
               quickid = e_minus_type
               Return
            Endif
         elseif ( (dedx_in .ge. line1) .and. (dedx_in .lt. line2) ) then
            If (rig_in .gt. 0.0) then
               quickid = pi_plus_type
               Return
            else
               quickid = pi_minus_type
               Return
            Endif
         elseif ( (dedx_in .ge. line2) .and.
     &            (dedx_in .lt. line3) ) then
            If ((dedx_in .ge. elwr) .and. (dedx_in .lt. eupr)) then
               If (rig_in .gt. 0.0) then
                  quickid = e_plus_type
                  Return
               else
                  quickid = e_minus_type
                  Return
               Endif
            else
               If (rig_in .gt. 0.0) then
                  quickid = pi_plus_type
                  Return
               else
                  quickid = pi_minus_type
                  Return
               Endif
            Endif
         elseif ( (dedx_in .ge. line3) .and.
     &            (dedx_in .lt. line4) ) then
            If (rig_in .gt. 0.0) then
               quickid = k_plus_type
               Return
            else
               quickid = k_minus_type
               Return
            Endif
         elseif ( (dedx_in .ge. line4) .and.
     &            (dedx_in .lt. line5) ) then
            If (rig_in .gt. 0.0) then
               quickid = proton_type
               Return
            else
               quickid = anti_proton_type
               Return
            Endif
         elseif (dedx_in .ge. line5) then
            If (rig_in .gt. 0.0) then
               quickid = 600
               Return
            else
               quickid = 700
               Return
            Endif
         Endif
      End
