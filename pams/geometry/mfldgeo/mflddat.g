#if 0
Module    MFLDDAT  is the realistic field data
author    Pavel Nevski
created   december 1, 1998

structure BFLD { version, char code, date, int kz, rmaxx, zmaxx, rrm, zz1, zz2}
structure BDAT { int N, Zi, Ri(20), Bzi(20), Bri(20) }
structure BFIT { int nr, int nzc, int nzs, rmax, zmax,
                 bzcorn, zterm(20), rterm(20), bn(20) }
integer   iprin


fill  BFLD(1)    ! real field
  version= 3        ! version
  code   = 'opt1'   ! fit version code
  date   = 22.10    ! fit date 
  kz     = 22       ! number of z lines
  rmaxx  = 270      ! maximum radius of extrapolated measurements
  zmaxx  = 290      ! maximum length of extrapolated measurements
  RRM    = 400      ! maximum radius of all fields
  ZZ1    = 270      ! length of measured field interpolation
  ZZ2    = 800      ! max length of all fields
*

Fill BFIT(1)  ! fitted measurments
  nr     = 20       ! number of R-terms in fit
  nzc    = 18       ! number of cos-term in fit
  nzs    = 2        ! number of sin-term in fit
  rmax   = 224.5    ! maximum field measured radius
  zmax   = 270      ! maximum field measured length
  bzcorn = 4930.042 ! measured Bz in the left upper corner
  zterm = { 81.8467, -81.1753,  12.8447, -18.8354,  4.51084,
           -7.19105,  1.77170, -1.90463, -1.32134, -5.61097,
            2.15714, -1.22481,  .774397, -.653669,  .557070,
           -.325353,  .313685, -.313419,  7.90497, -7.08619 }    ! z amplitude
  rterm = {37.10693, 96.20678,-255.50438,-236.56873, 58.51688,
          116.89392,-45.77096,-15.01237,  1.06193,  5.04434,
           -6.28163,  3.71463, -3.39107,  2.47896, -7.10708,
            4.78629, -6.68983, -1.52063, -4.09440, -0.46484 }    ! R amplitudes
  bn    = {0.765480,  1.757095,  2.754567,  3.753362,  4.752658,
           5.752198,  6.751873,  7.751632,  8.751446,  9.751298,
          10.751177, 11.751078, 12.750993, 13.750920, 14.750858,
          15.750804, 16.750755, 17.750713, 18.750675, 19.750641 }! bessel roots
*

Fill  BDAT(1) ! calculated STAR field
  N  =     15   ! Number of field points
  Zi =  270.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 100.0, 125.0, 150.0, 175.0,
        200.0, 225.0, 250.0, 275.0, 300.0, 375.0, 400.0 } ! Radius
  Bzi={4704.6,4768.0,4946.6,5148.3,5128.6,4927.3,4844.9,4830.1,
       4823.3,4858.0,5110.9,3402.4, -18.1, -13.6, -25.0 } ! Axial  field
  Bri= {  0.0, 131.3, 188.4,  74.1,-148.6,-164.8, -53.2,  23.8,
         97.4, 213.7, 329.3,  75.3,  18.2, -44.3, -36.5 } ! Radial field
*

Fill  BDAT ! calculated STAR field
  N  =     15   ! Number of field points
  Zi =  280.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 100.0, 125.0, 150.0, 175.0,
        200.0, 225.0, 250.0, 275.0, 300.0, 375.0, 400.0 } ! Radius
  Bzi={4568.8,4649.8,4898.6,5241.5,5234.5,4883.9,4806.5,4802.4,
       4781.7,4771.8,5057.8,3504.2,-144.8, -15.0, -28.0 } ! Axial  field
  Bri= {  0.0, 188.6, 297.5, 151.9,-241.2,-242.5, -60.1,  19.5,
         92.3, 244.3, 541.5, 396.8,  83.6, -49.9, -40.6 } ! Radial field
*

Fill  BDAT ! calculated STAR field
  N  =     15   ! Number of field points
  Zi =  290.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 100.0, 125.0, 150.0, 175.0,
        200.0, 225.0, 250.0, 275.0, 300.0, 375.0, 400.0 } ! Radius
  Bzi={4383.8,4478.4,4801.1,5378.7,5431.1,4771.4,4765.5,4778.2,
       4741.4,4651.9,4852.9,3684.4,   6.9, -16.9, -31.6 } ! Axial  field
  Bri= {  0.0, 260.2, 456.5, 312.9,-414.5,-349.8, -51.7,  14.4,
         74.7, 234.0, 858.0, 726.3, 355.0, -56.5, -45.0 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =     15   ! Number of field points
  Zi =  300.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 100.0, 125.0, 150.0, 175.0,
        200.0, 225.0, 250.0, 275.0, 300.0, 375.0, 400.0 } ! Radius
  Bzi={4142.1,4240.8,4614.8,5546.4,5829.1,4450.0,4737.6,4761.4,
       4711.3,4534.1,4231.0,4067.5,-880.0, -19.3, -36.2 } ! Axial  field
  Bri= {  0.0, 341.1, 669.5, 661.0,-766.7,-480.9, -24.5,   8.8,
         43.5, 149.9,1333.6, 999.3,  53.6, -64.2, -49.8 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =     15   ! Number of field points
  Zi =  310.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 100.0, 125.0, 150.0, 175.0,
        200.0, 225.0, 250.0, 275.0, 300.0, 375.0, 400.0 } ! Radius
  Bzi={3842.7,3930.2,4292.5,5589.1,6643.0,3236.8,4733.0,4755.1,
       4699.4,4485.0,1931.8,4782.0,  50.2, -22.8, -42.0 } ! Axial  field
  Bri= {  0.0, 421.2, 915.6,1382.6,-1482.8,-1019.7,  1.2,  2.0,
          1.9,  -2.3,2069.4, 791.7, 240.6, -73.6, -54.9 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =      8   ! Number of field points
  Zi =  320.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 100.0, 125.0, 375.0, 400.0 } ! Radius
  Bzi={3491.2,3552.1,3807.3,4923.7,7889.6,1983.9, -28.0, -49.4 } ! Axial  field
  Bri= {  0.0, 485.7,1133.5,2502.8, -38.8,-174.8, -85.1, -60.0 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =      7   ! Number of field points
  Zi =  330.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 250.0, 375.0, 400.0 } ! Radius
  Bzi={3105.3,3127.0,3200.4,3268.9,  -3.5, -36.6, -59.0 } ! Axial  field
  Bri= {  0.0, 521.1,1246.1,3029.5,9199.2, -99.4, -64.5 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =      6   ! Number of field points
  Zi =  340.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 375.0, 400.0 } ! Radius
  Bzi={2706.4,2686.8,2574.5,1826.7, -51.8, -71.0 } ! Axial  field
  Bri= {  0.0, 520.6,1218.1,2485.3,-116.9, -67.3 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =      6   ! Number of field points
  Zi =  350.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 375.0, 400.0 } ! Radius
  Bzi={2317.7,2264.6,2026.3,1142.6, -80.8, -85.1 } ! Axial  field
  Bri= {  0.0, 487.6,1082.3,1787.2,-133.8, -67.0 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =      8   ! Number of field points
  Zi =  360.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 100.0, 250.0, 375.0, 400.0 } ! Radius
  Bzi={1958.5,1885.6,1595.6, 829.2,-563.7,4895.8,-127.6, -99.8 } ! Axial  field
  Bri= {  0.0, 432.4, 901.7,1265.8, 788.0,9507.4,-134.0, -62.2 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =     17   ! Number of field points
  Zi =  370.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 100.0, 125.0, 150.0, 175.0, 200.0, 
        225.0, 250.0, 275.0, 300.0, 325.0, 350.0, 375.0, 400.0 } ! Radius
  Bzi={1637.8,1562.2,1276.7, 678.9,  15.7, 251.6, 384.9, 503.7, 683.3, 
       1087.1,1868.1,-1320.5,-593.9,-391.5,-345.9,-168.2,-112.9 } ! Axial field
  Bri= {  0.0, 367.3, 720.6, 900.1, 421.6,  60.4,  37.1,  44.5,  79.7,
        229.6,2339.4, 654.6, 114.6,  35.9, -30.0,-101.8, -52.4 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =     17   ! Number of field points
  Zi =  380.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 100.0, 125.0, 150.0, 175.0, 200.0,  
        225.0, 250.0, 275.0, 300.0, 325.0, 350.0, 375.0, 400.0 } ! Radius
  Bzi={1373.5,1296.6,1045.5, 603.2, 221.4, 278.6, 382.7, 488.2, 638.7,
        892.4, 708.6,-709.9,-515.0,-364.7,-293.1,-181.5,-122.1 } ! Axial  field
  Bri= {  0.0, 302.3, 563.3, 650.3, 369.7, 120.0,  79.6,  96.2, 169.1,
        430.1,1454.7, 860.7, 228.6,  77.5, -10.8, -60.2, -39.4 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =     17   ! Number of field points
  Zi =  390.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 100.0, 125.0, 150.0, 175.0, 200.0, 
        225.0, 250.0, 275.0, 300.0, 325.0, 350.0, 375.0, 400.0 } ! Radius
  Bzi={1151.2,1083.6, 877.2, 557.6, 308.9, 305.2, 377.6, 463.3, 573.3, 
        684.5, 377.5,-376.2,-415.2,-326.0,-258.2,-179.8,-126.9 } ! Axial  field
  Bri= {  0.0, 243.7, 437.7, 486.1, 319.9, 155.1, 115.2, 139.4, 232.4, 
        494.6,1019.1, 751.4, 289.6, 112.2,  19.4, -26.7, -25.0 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =     17   ! Number of field points
  Zi =  400.0   ! distance to  Z=0
  Ri = {  0.0,  25.0,  50.0,  75.0, 100.0, 125.0, 150.0, 175.0, 200.0, 
        225.0, 250.0, 275.0, 300.0, 325.0, 350.0, 375.0, 400.0 } ! Radius
  Bzi= {971.6, 914.8, 751.6, 520.8, 348.0, 323.7, 369.1, 432.0, 500.9, 
        520.4, 251.2,-214.3,-320.8,-282.3,-230.2,-171.7,-127.7 } ! Axial  field
  Bri= {  0.0, 194.5, 341.1, 375.8, 277.5, 171.7, 142.1, 172.1, 269.4, 
        486.6, 769.0, 624.1, 308.0, 137.2,  44.9,  -1.4, -11.2 } ! Radial field


Fill  BDAT ! calculated STAR field
  N  =      9   ! Number of field points
  Zi =  450.0   ! distance to  Z=0
  Ri = {  0.0,  50.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0 } ! Radius
  Bzi= {481.5, 423.2, 325.1, 283.8, 242.6,  88.2, -85.2,-123.1,-103.9 } ! Axial 
  Bri= {  0.0, 119.3, 157.5, 174.6, 248.4, 314.8, 220.8,  95.4,  32.3 } ! Radial


Fill  BDAT ! calculated STAR field
  N  =      9   ! Number of field points
  Zi =  500.0   ! distance to  Z=0
  Ri = {  0.0,  50.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0 } ! Radius
  Bzi= {291.3, 273.2, 234.4, 192.6, 136.7,  53.6, -25.6, -64.6, -72.5 } ! Axial 
  Bri= {  0.0,  60.7, 103.2, 135.9, 168.1, 177.4, 140.6,  84.7,  41.9 } ! Radial


Fill  BDAT ! calculated STAR field
  N  =      9   ! Number of field points
  Zi =  550.0   ! distance to  Z=0
  Ri = {  0.0,  50.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0 } ! Radius
  Bzi= {190.4, 181.9, 159.6, 127.7,  86.0,  37.1,  -7.3, -35.9, -48.9 } ! Axial
  Bri= {  0.0,  37.8,  69.7,  94.3, 110.3, 110.8,  92.6,  64.2,  37.3 } ! Radial


Fill  BDAT ! calculated STAR field
  N  =      9   ! Number of field points
  Zi =  600.0   ! distance to  Z=0
  Ri = {  0.0,  50.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0 } ! Radius
  Bzi= {126.9, 121.7, 107.1,  84.9,  56.8,  26.4,  -1.2, -21.2, -32.9 } ! Axial 
  Bri= {  0.0,  25.0,  46.9,  63.4,  72.6,  72.2,  62.3,  46.3,  29.2 } ! Radial


Fill  BDAT ! calculated STAR field
  N  =      9   ! Number of field points
  Zi =  650.0   ! distance to  Z=0
  Ri = {  0.0,  50.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0 } ! Radius
  Bzi= { 84.8,  81.4,  71.7,  56.8,  38.3,  18.7,   0.8, -13.1, -22.2 } ! Axial
  Bri= {  0.0,  16.7,  31.4,  42.4,  48.2,  48.1,  42.4,  32.8,  21.6 } ! Radial


Fill  BDAT ! calculated STAR field
  N  =      9   ! Number of field points
  Zi =  700.0   ! distance to  Z=0
  Ri = {  0.0,  50.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0 } ! Radius
  Bzi= { 56.7,  54.4,  47.9,  38.0,  25.9,  13.1,   1.3,  -8.3, -15.0 } ! Axial
  Bri= {  0.0,  11.2,  21.1,  28.4,  32.4,  32.5,  29.1,  23.0,  15.5 } ! Radial


Fill  BDAT ! calculated STAR field
  N  =      9   ! Number of field points
  Zi =  750.0   ! distance to  Z=0
  Ri = {  0.0,  50.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0 } ! Radius
  Bzi= { 37.7,  36.2,  31.9,  25.4,  17.5,   9.1,   1.2,  -5.4, -10.1 } ! Axial
  Bri= {  0.0,   7.6,  14.3,  19.3,  22.0,  22.2,  20.1,  16.1,  11.0 } ! Radial


Fill  BDAT ! calculated STAR field
  N  =      9   ! Number of field points
  Zi =  800.0   ! distance to  Z=0
  Ri = {  0.0,  50.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0 } ! Radius
  Bzi= { 24.8,  23.8,  21.0,  16.8,  11.6,   6.1,   0.9,  -3.5,  -6.7 } ! Axial
  Bri= {  0.0,   5.2,   9.8,  13.3,  15.2,  15.4,  14.1,  11.4,   7.9 } ! Radial

end
#endif




