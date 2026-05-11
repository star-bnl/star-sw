
      BLOCK DATA DT_NONAME
 
      IMPLICIT NONE
      SAVE 
 
C slope parameters for HADRIN interactions
      INCLUDE 'inc/hnslop'
      INCLUDE 'inc/hnredv'
 
C     DATAS     DATAS    DATAS      DATAS     DATAS
C******          *********
      DATA IKIi/0 , 15 , 41 , 67 , 82 , 93 , 110 , 133 , 148 , 159 , 
     &     172 , 183 , 207 , 224 , 241 , 252 , 268/
      DATA IEIi/0 , 21 , 46 , 71 , 92 , 109 , 126 , 143 , 160 , 173 , 
     &     186 , 199 , 220 , 241 , 262 , 279 , 296/
      DATA IRIi/0 , 315 , 965 , 1615 , 1930 , 2117 , 2406 , 2797 , 
     &     3052 , 3195 , 3364 , 3507 , 4011 , 4368 , 4725 , 4912 , 5184/
 
C
C     MASSES FOR THE SLOPE B(M) IN GEV
C     SLOPE B(M) FOR AN MESONIC SYSTEM
C     SLOPE B(M) FOR A BARYONIC SYSTEM
 
C
      DATA SM , BBM , BBB/0.8D0 , 0.85D0 , 0.9D0 , 0.95D0 , 1.D0 , 
     &     1.05D0 , 1.1D0 , 1.15D0 , 1.2D0 , 1.25D0 , 1.3D0 , 1.35D0 , 
     &     1.4D0 , 1.45D0 , 1.5D0 , 1.55D0 , 1.6D0 , 1.65D0 , 1.7D0 , 
     &     1.75D0 , 1.8D0 , 1.85D0 , 1.9D0 , 1.95D0 , 2.D0 , 15.6D0 , 
     &     14.95D0 , 14.3D0 , 13.65D0 , 13.D0 , 12.35D0 , 11.7D0 , 
     &     10.85D0 , 10.D0 , 9.15D0 , 8.3D0 , 7.8D0 , 7.3D0 , 7.25D0 , 
     &     7.2D0 , 6.95D0 , 6.7D0 , 6.6D0 , 6.5D0 , 6.3D0 , 6.1D0 , 
     &     5.85D0 , 5.6D0 , 5.35D0 , 5.1D0 , 15.D0 , 15.D0 , 15.D0 , 
     &     15.D0 , 15.D0 , 15.D0 , 15.D0 , 14.2D0 , 13.4D0 , 12.6D0 , 
     &     11.8D0 , 11.2D0 , 10.6D0 , 9.8D0 , 9.D0 , 8.25D0 , 7.5D0 , 
     &     6.25D0 , 5.D0 , 4.5D0 , 5*4.D0/
C
      END BLOCK DATA
