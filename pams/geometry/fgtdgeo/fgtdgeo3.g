      MODULE FGTDGEO3    forward GEM tracking detector for 2012 , 2013
      Created   12/20/2011
      Author Jan Balewski MIT, Wei-Ming Zhang KSU, Willie Leight MIT (material mixes)
      +CDE,agecom,gcunit,gconst.
      CONTENT FGTM, FGTD, FGTQ,  FGTH,
      FGVN, FGZC, FGVG, FGVR, FGVE,
      FGQA, FGQB, FGQC, FGQD, FGQE, FGQF,
      FGXA, FGXB, FGXC, FGXD,
      FGWA, FGWB, FGWC, FGWD, FGWE,
      FGCT, FGCN, FGCM
      real *4 coolTubeInR/10.25/

      real *4 coolTubeDZ/150/

      real *4 diskInR/10.35/

      real *4 diskOutR/39.4/

      real *4 flatOutR/37.0/

      real *4 flatPerpAng/31.0/

      real *4 quadTiltAng/-15.0/

      real *4 gridArcBigR/29.15/

      real *4 gridArcSmallR/20.63/

      real *4 gridRayLongPhi/60./

      real *4 gridRayShortPhi/30./

      real *4 gridWidth/0.2/

      real *4 frameWidth/1.15/

      real *4 frameXYoff/0.05/

      real *4 frameDZ/0.198/

      real *4 volNomexDZ/1.288/

      real *4 volSensDZ/0.312/

      real *4 volGemDZ/0.612/

      real *4 volReadDZ/0.246/

      real *4 volElecDZ/4.3/

      real *4 elecWidth/0.16/

      real *4 elecAdy/1.4/

      real *4 elecCdy/4.5/

      real *4 elecDang/9./

      real *4 elecElen/22.5/

      REAL centerZ,fgtLenZ,diskLenZ,     flatAng1,flatAng2,quad,disk,     xx,diskZ,
            k,zzFGZ, xxFGX,xxFGQ, xxFGZ

      STRUCTURE FGGG { FgstConfig}
      STRUCTURE FGST { config,int ndisk,int nQuad,zDiscA(6)}
      FILL FGGG		! FGT Geometry Control Structure
      FgstConfig = 1.0 ! selection of no. of disks and quadrants
      ENDFILL
      FILL FGST		! FGT Geometry  for year 2012
      config = 1.0 ! versioning of the FGST geometry data
      ndisk = 6 ! number of disks
      nQuad = 4 ! number quadrants in a disks
      zdiscA  = {   67.3990,   77.8765,   87.0840,   97.4821, 
                   108.9121,  118.9927} !   z-center of disk senstive volume from center in STAR , as measured in 2011-10  
      ENDFILL
      FILL FGST		! FGT Geometry with one quadrant in one disk
      config = 2.0 ! versioning of the FGST geometry data
      ndisk = 6 ! number of disks
      nQuad = 4 ! number quadrants in a disks
      zdiscA  = {   70.,   80.,   90.,  100.,  110.,  120.} !   z-center of disk senstive volume from center in STAR , ideal location 
      ENDFILL
      COMPONENT Si A=28.08 Z=14 W=0.281
      COMPONENT O A=16 Z=8 W=0.467
      COMPONENT C A=12 Z=6 W=0.220
      COMPONENT H A=1 Z=1 W=0.032
      MIXTURE FR4 dens=1.80
      COMPONENT O A=16 Z=8 W=0.062
      COMPONENT C A=12 Z=6 W=0.892
      COMPONENT H A=1 Z=1 W=0.019
      COMPONENT Cl A=35.5 Z=17 W=0.027
      MIXTURE CFRPMix dens=1.78
      COMPONENT O A=16 Z=8 W=0.142
      COMPONENT C A=12 Z=6 W=0.637
      COMPONENT H A=1 Z=1 W=0.097
      COMPONENT N A=14 Z=7 W=0.124
      MIXTURE Nylon dens=1.15
      COMPONENT O A=16 Z=8 W=0.129
      COMPONENT C A=12 Z=6 W=0.579
      COMPONENT H A=1 Z=1 W=0.088
      COMPONENT N A=14 Z=7 W=0.112
      COMPONENT Al A=27 Z=13 W=0.092
      MIXTURE CoolMix dens=1.214
      COMPONENT O A=16 Z=8 W=0.090
      COMPONENT C A=12 Z=6 W=0.796
      COMPONENT H A=1 Z=1 W=0.023
      COMPONENT Cl A=35.5 Z=17 W=0.019
      COMPONENT N A=14 Z=7 W=0.025
      COMPONENT Al A=27 Z=13 W=0.002
      COMPONENT Cu A=63.5 Z=29 W=0.039
      COMPONENT Ar A=39.9 Z=18 W=0.006
      MIXTURE NomexMix dens=0.090
      COMPONENT O A=16 Z=8 W=0.095
      COMPONENT C A=12 Z=6 W=0.301
      COMPONENT H A=1 Z=1 W=0.011
      COMPONENT N A=14 Z=7 W=0.032
      COMPONENT Cu A=63.5 Z=29 W=0.547
      COMPONENT Ar A=39.9 Z=18 W=0.014
      MIXTURE GemMix dens=0.079
      COMPONENT O A=16 Z=8 W=0.160
      COMPONENT C A=12 Z=6 W=0.508
      COMPONENT H A=1 Z=1 W=0.020
      COMPONENT N A=14 Z=7 W=0.051
      COMPONENT Al A=27 Z=13 W=0.003
      COMPONENT Cu A=63.5 Z=29 W=0.255
      COMPONENT Ar A=39.9 Z=18 W=0.003
      MIXTURE ReadMix dens=0.356
      COMPONENT Si A=28.1 Z=14 W=0.191
      COMPONENT O A=16 Z=8 W=0.339
      COMPONENT C A=12 Z=6 W=0.213
      COMPONENT H A=1 Z=1 W=0.028
      COMPONENT Cl A=35.5 Z=17 W=0.009
      COMPONENT Cu A=63.5 Z=29 W=0.122
      COMPONENT Pb A=207 Z=82 W=0.039
      COMPONENT Sn A=118.7 Z=50 W=0.059
      MIXTURE APVMix dens=2.535
      COMPONENT Si A=28.1 Z=14 W=0.113
      COMPONENT O A=16 Z=8 W=0.241
      COMPONENT C A=12 Z=6 W=0.248
      COMPONENT H A=1 Z=1 W=0.023
      COMPONENT Cu A=63.5 Z=29 W=0.109
      COMPONENT Fe A=55.8 Z=26 W=0.019
      COMPONENT Cr A=52.0 Z=24 W=0.008
      COMPONENT Pb A=207 Z=82 W=0.096
      COMPONENT Sn A=118.7 Z=50 W=0.143
      MIXTURE HVMix dens=3.009
      COMPONENT Si A=28.1 Z=14 W=0.122
      COMPONENT O A=16 Z=8 W=0.225
      COMPONENT C A=12 Z=6 W=0.297
      COMPONENT H A=1 Z=1 W=0.023
      COMPONENT N A=14 Z=7 W=0.009
      COMPONENT Cu A=63.5 Z=29 W=0.045
      COMPONENT Fe A=55.8 Z=26 W=0.055
      COMPONENT Ni A=58.7 Z=28 W=0.022
      COMPONENT Pb A=207 Z=82 W=0.081
      COMPONENT Sn A=118.7 Z=50 W=0.121
      MIXTURE ConMix dens=1.816
      COMPONENT Si A=28.1 Z=14 W=0.141
      COMPONENT O A=16 Z=8 W=0.196
      COMPONENT C A=12 Z=6 W=0.419
      COMPONENT H A=1 Z=1 W=0.008
      COMPONENT Cu A=63.5 Z=29 W=0.156
      COMPONENT Pb A=207 Z=82 W=0.032
      COMPONENT Sn A=118.7 Z=50 W=0.048
      MIXTURE TermMix dens=2.55
      COMPONENT Cu A=63.5 Z=29 W=0.63
      COMPONENT Zn A=65.4 Z=30 W=0.37
      MIXTURE Brass dens=8.4
      COMPONENT Ar A=39.95 Z=18. W=0.700
      COMPONENT O A=16. Z=8. W=0.218
      COMPONENT C A=12.01 Z=6. W=0.082
      MIXTURE ArCO2Mix isvol=1 dens=0.0018015
      use FGGG
      use FGST config=FGGG_FgstConfig
      diskLenZ=volNomexDZ+volSensDZ+volGemDZ+volReadDZ+volElecDZ
      fgtLenZ = coolTubeDZ
      centerZ = FGST_zdiscA(1) +fgtLenZ/2.0 -( volNomexDZ+volSensDZ/2.)
      xx=acos(flatOutR/diskOutR)
      flatAng1=flatPerpAng-xx/degrad
      flatAng2=flatPerpAng+xx/degrad
      CREATE FGTM
      POSITION FGTM in IDSM z=centerZ 
      ! ---------------------------------------------------------------------------------- FGTM
      BLOCK FGTM   mother volume for FGT assembly
      ATTRIBUTE FGTM seen=0 colo=1
      MATERIAL Air
      SHAPE tube rmin=coolTubeInR rmax=diskOutR,
            dz=fgtLenZ/2.0
      CREATE FGCT
      POSITION FGCT 
      CREATE FGCN
      POSITION FGCN z=-fgtLenZ/2.0+0.64/2. 
      DO disk = 1, FGST_ndisk
      diskZ=-fgtLenZ/2.+1.0 +diskLenZ/2. + FGST_zdiscA(disk)- FGST_zdiscA(1)
      IF ( (FGST_config.eq.2.0).or.(disk.le.1) ) THEN
      CREATE FGTD
      POSITION FGTD z=diskZ alphaz=quadTiltAng ncopy=disk 
      ELSE
      CREATE FGTH
      POSITION FGTH z=diskZ alphaz=quadTiltAng ncopy=disk 
      ENDIF
      END DO
      DO disk = 2, FGST_ndisk
      diskZ=-fgtLenZ/2. + 1.0 + FGST_zdiscA(disk)- FGST_zdiscA(1) -1.28/2.
      CREATE FGCM
      POSITION FGCM z=diskZ 
      END DO
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGCT
      BLOCK FGCT   inner cooling tube
      ATTRIBUTE FGCT seen=1 colo=6
      MATERIAL CFRPMix
      SHAPE tube rmin=coolTubeInR rmax=coolTubeInR+0.05,
            dz=coolTubeDZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGCN
      BLOCK FGCN   nylon 1st ring
      ATTRIBUTE FGCN seen=1 colo=6
      MATERIAL Nylon
      SHAPE tube rmin=10.35 rmax=11.5 dz=0.64/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGCM
      BLOCK FGCM   nylon and Al ring
      ATTRIBUTE FGCM seen=1 colo=6
      MATERIAL CoolMix
      SHAPE tube rmin=10.35 rmax=11.54 dz=1.28/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGTD
      BLOCK FGTD   mother volume for FGT disk
      ATTRIBUTE FGTD seen=1 colo=5
      MATERIAL Air
      SHAPE tube rmin=diskInR rmax=diskOutR dz=diskLenZ/2.0
      DO quad = 1, FGST_nQuad
      CREATE FGTQ
      POSITION FGTQ z=0. alphaz=(1-quad)*90. 
      END DO
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGTH
      BLOCK FGTH   mother volume for FGT disk
      ATTRIBUTE FGTH seen=1 colo=7
      MATERIAL Air
      SHAPE tube rmin=diskInR rmax=diskOutR dz=diskLenZ/2.0
      DO quad = 1, 2
      CREATE FGTQ
      POSITION FGTQ z=0. alphaz=(1-quad)*90. 
      END DO
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGTQ
      BLOCK FGTQ   quadrant
      ATTRIBUTE FGTQ seen=0 colo=4
      MATERIAL Air
      SHAPE tubs rmin=diskInR rmax=diskOutR phi1=0.,
            phi2=90. dz=diskLenZ/2.
      CREATE FGVN
      POSITION FGVN z=-diskLenZ/2.+volNomexDZ/2. 
      CREATE FGZC
      POSITION FGZC z=-diskLenZ/2.+volNomexDZ+volSensDZ/2. 
      CREATE FGVG
      POSITION FGVG z=-diskLenZ/2.+volNomexDZ+volSensDZ+volGemDZ/2. 
      CREATE FGVR
      POSITION FGVR z=-diskLenZ/2.+volNomexDZ+volSensDZ+volGemDZ+volReadDZ/2. 
      CREATE FGVE
      POSITION FGVE  _
	z=-diskLenZ/2.+volNomexDZ+volSensDZ+volGemDZ+volReadDZ+volElecDZ/2. 
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGVN
      BLOCK FGVN   quad nomex mixture volume
      ATTRIBUTE FGVN seen=0 colo=5
      MATERIAL NomexMix
      SHAPE tubs rmin=diskInR rmax=diskOutR phi1=0.,
            phi2=90. dz=volNomexDZ/2.
      DO k = 0, 2
      zzFGZ=(-volNomexDZ+frameDZ)/2.
      IF ( k.eq.1 ) THEN
      zzFGZ=(volNomexDZ-frameDZ)/2.-0.007
      ENDIF
      IF ( k.eq.2 ) THEN
      zzFGZ=(volNomexDZ-frameDZ)/2.-0.014 -  frameDZ
      ENDIF
      CREATE FGQA
      POSITION FGQA x=frameWidth/2.+frameXYoff y=(diskOutR+diskInR)/2. z=zzFGZ 
      CREATE FGQB
      POSITION FGQB z=zzFGZ 
      CREATE FGQC
      POSITION FGQC x=(diskOutR+diskInR)/2. y=frameWidth/2.+frameXYoff z=zzFGZ 
      CREATE FGQD
      POSITION FGQD z=zzFGZ 
      CREATE FGQE
      POSITION FGQE x=(flatOutR-frameWidth/2.)*cos(flatPerpAng*degrad)  _
	y=(flatOutR-frameWidth/2.)*sin(flatPerpAng*degrad) z=zzFGZ  _
	alphaz=flatPerpAng-90. 
      CREATE FGQF
      POSITION FGQF z=zzFGZ 
      END DO
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGZC
      BLOCK FGZC   sensitive volume
      ATTRIBUTE FGZC seen=1 colo=1
      MATERIAL ArCO2Mix
      MATERIAL Sensitive_fgt_gas isvol=1
      SHAPE tubs rmin=diskInR rmax=diskOutR phi1=0.,
            phi2=90. dz=volSensDZ/2.
      HITS FGZC ZZ:0.001:S YY:0.001: XX:0.001: ptot:16:(0,100) cx:10: cy:10: cz:10:,
            sleng:16:(0,500) tof:16:(0,1.0E-6) step:0.01: eloss:16:(0,0.001)
      DO k = 0, 1
      zzFGZ=(-volSensDZ+frameDZ)/2. * (1.-2.*k)
      CREATE FGQA
      POSITION FGQA x=frameWidth/2.+frameXYoff y=(diskOutR+diskInR)/2. z=zzFGZ 
      CREATE FGQB
      POSITION FGQB z=zzFGZ 
      CREATE FGQC
      POSITION FGQC x=(diskOutR+diskInR)/2. y=frameWidth/2.+frameXYoff z=zzFGZ 
      CREATE FGQD
      POSITION FGQD z=zzFGZ 
      CREATE FGQE
      POSITION FGQE x=(flatOutR-frameWidth/2.)*cos(flatPerpAng*degrad)  _
	y=(flatOutR-frameWidth/2.)*sin(flatPerpAng*degrad) z=zzFGZ  _
	alphaz=flatPerpAng-90. 
      CREATE FGQF
      POSITION FGQF z=zzFGZ 
      CREATE FGXA
      POSITION FGXA z=zzFGZ 
      CREATE FGXB
      POSITION FGXB z=zzFGZ 
      CREATE FGXC
      xxFGZ=(diskOutR+diskInR)/2.
      POSITION FGXC x=xxFGZ*cos(gridRayLongPhi*degrad)  _
	y=xxFGZ*sin(gridRayLongPhi*degrad) z=zzFGZ alphaz=gridRayLongPhi 
      CREATE FGXD
      xxFGZ=(flatOutR+diskInR)/2.
      POSITION FGXD x=xxFGZ*cos(gridRayShortPhi*degrad)  _
	y=xxFGZ*sin(gridRayShortPhi*degrad) z=zzFGZ alphaz=gridRayShortPhi 
      END DO
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGVG
      BLOCK FGVG   quad 3GEM  mixture volume
      ATTRIBUTE FGVG seen=0 colo=6
      MATERIAL GemMix
      SHAPE tubs rmin=diskInR rmax=diskOutR phi1=0.,
            phi2=90. dz=volGemDZ/2.
      DO k = 0, 2
      zzFGZ=(-volGemDZ+frameDZ-0.01)/2. * (-1+k)
      CREATE FGQA
      POSITION FGQA x=frameWidth/2.+frameXYoff y=(diskOutR+diskInR)/2. z=zzFGZ 
      CREATE FGQB
      POSITION FGQB z=zzFGZ 
      CREATE FGQC
      POSITION FGQC x=(diskOutR+diskInR)/2. y=frameWidth/2.+frameXYoff z=zzFGZ 
      CREATE FGQD
      POSITION FGQD z=zzFGZ 
      CREATE FGQE
      POSITION FGQE x=(flatOutR-frameWidth/2.)*cos(flatPerpAng*degrad)  _
	y=(flatOutR-frameWidth/2.)*sin(flatPerpAng*degrad) z=zzFGZ  _
	alphaz=flatPerpAng-90. 
      CREATE FGQF
      POSITION FGQF z=zzFGZ 
      CREATE FGXA
      POSITION FGXA z=zzFGZ 
      CREATE FGXB
      POSITION FGXB z=zzFGZ 
      CREATE FGXC
      xxFGZ=(diskOutR+diskInR)/2.
      POSITION FGXC x=xxFGZ*cos(gridRayLongPhi*degrad)  _
	y=xxFGZ*sin(gridRayLongPhi*degrad) z=zzFGZ alphaz=gridRayLongPhi 
      CREATE FGXD
      xxFGZ=(flatOutR+diskInR)/2.
      POSITION FGXD x=xxFGZ*cos(gridRayShortPhi*degrad)  _
	y=xxFGZ*sin(gridRayShortPhi*degrad) z=zzFGZ alphaz=gridRayShortPhi 
      END DO
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGVR
      BLOCK FGVR   quad readout mixture volume
      ATTRIBUTE FGVR seen=0 colo=5
      MATERIAL ReadMix
      SHAPE tubs rmin=diskInR rmax=diskOutR phi1=0.,
            phi2=90. dz=volReadDZ/2.
      zzFGZ= - volReadDZ/2. + frameDZ/1.5
      CREATE FGQA
      POSITION FGQA x=frameWidth/2.+frameXYoff y=(diskOutR+diskInR)/2. z=zzFGZ 
      CREATE FGQB
      POSITION FGQB z=zzFGZ 
      CREATE FGQC
      POSITION FGQC x=(diskOutR+diskInR)/2. y=frameWidth/2.+frameXYoff z=zzFGZ 
      CREATE FGQD
      POSITION FGQD z=zzFGZ 
      CREATE FGQE
      POSITION FGQE x=(flatOutR-frameWidth/2.)*cos(flatPerpAng*degrad)  _
	y=(flatOutR-frameWidth/2.)*sin(flatPerpAng*degrad) z=zzFGZ  _
	alphaz=flatPerpAng-90. 
      CREATE FGQF
      POSITION FGQF z=zzFGZ 
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGVE
      BLOCK FGVE   quad electronics volume
      ATTRIBUTE FGVE seen=0 colo=7
      MATERIAL Air
      SHAPE tubs rmin=diskInR rmax=diskOutR phi1=0.,
            phi2=90. dz=volElecDZ/2.
      CREATE FGWA
      POSITION FGWA x=diskInR+frameWidth*0.7 y=elecAdy/2. 
      CREATE FGWA
      POSITION FGWA x=elecAdy/2. y=diskInR+frameWidth*0.7 alphaz=90. 
      CREATE FGWB
      POSITION FGWB x=(diskOutR+diskInR)/2. y=frameXYoff+elecWidth/2. 
      CREATE FGWB
      POSITION FGWB x=frameXYoff+elecWidth/2. y=(diskOutR+diskInR)/2. alphaz=90. 
      CREATE FGWC
      POSITION FGWC x=diskOutR-frameWidth*0.7 y=elecCdy/2. 
      CREATE FGWC
      POSITION FGWC x=elecCdy/2. y=diskOutR-frameWidth*0.7 alphaz=90. 
      CREATE FGWD
      POSITION FGWD x=(diskOutR-frameWidth/2.)*cos(elecDang*degrad)  _
	y=(diskOutR-frameWidth/2.)*sin(elecDang*degrad) alphax=90. 
      CREATE FGWD
      POSITION FGWD x=(diskOutR-frameWidth/2.)*sin(elecDang*degrad)  _
	y=(diskOutR-frameWidth/2.)*cos(elecDang*degrad) alphax=90. alphaz=90. 
      CREATE FGWE
      POSITION FGWE x=(flatOutR-frameWidth/2.)*cos(flatPerpAng*degrad)  _
	y=(flatOutR-frameWidth/2.)*sin(flatPerpAng*degrad) alphaz=flatPerpAng-90. 
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGQA
      BLOCK FGQA   A-spacer frame
      ATTRIBUTE FGQA seen=1 colo=4
      MATERIAL FR4
      SHAPE box dx=frameWidth/2. dy=(diskOutR-diskInR-frameWidth)/2.,
            dz=frameDZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGQB
      BLOCK FGQB   B-spacer frame, arc
      ATTRIBUTE FGQB seen=1 colo=4
      MATERIAL FR4
      xxFGQ=frameXYoff/diskInR/degrad
      SHAPE tubs rmin=diskInR rmax=diskInR+frameWidth,
            phi1=xxFGQ phi2=90.-xxFGQ dz=frameDZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGQC
      BLOCK FGQC   C-spacer frame, bar
      ATTRIBUTE FGQC seen=1 colo=4
      MATERIAL FR4
      SHAPE box dx=(diskOutR-diskInR-frameWidth)/2.,
            dy=frameWidth/2. dz=frameDZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGQD
      BLOCK FGQD   D-spacer frame, arc
      ATTRIBUTE FGQD seen=1 colo=4
      MATERIAL FR4
      xxFGQ=frameXYoff/diskOutR/degrad
      SHAPE tubs rmin=diskOutR-frameWidth rmax=diskOutR,
            phi1=xxFGQ phi2=flatAng1 dz=frameDZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGQE
      BLOCK FGQE   E-spacer frame, flat
      ATTRIBUTE FGQE seen=1 colo=4
      MATERIAL FR4
      SHAPE box dx=diskOutR*sin((flatAng2-flatAng1)*degrad/2.),
            dy=frameWidth/2. dz=frameDZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGQF
      BLOCK FGQF   F-spacer frame,  arc
      ATTRIBUTE FGQF seen=1 colo=4
      MATERIAL FR4
      xxFGQ=frameXYoff/diskOutR/degrad
      SHAPE tubs rmin=diskOutR-frameWidth rmax=diskOutR,
            phi1=flatAng2 phi2=90.-xxFGQ dz=frameDZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGXA
      BLOCK FGXA   A-grid, arc
      ATTRIBUTE FGXB seen=1 colo=3
      MATERIAL FR4
      xxFGX=frameXYoff/gridArcBigR/degrad
      SHAPE tubs rmin=gridArcBigR rmax=gridArcBigR+gridWidth,
            phi1=xxFGX phi2=90.-xxFGX dz=frameDZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGXB
      BLOCK FGXB   B-grid, arc
      ATTRIBUTE FGXB seen=1 colo=3
      MATERIAL FR4
      xxFGX=frameXYoff/gridArcSmallR/degrad
      SHAPE tubs rmin=gridArcSmallR rmax=gridArcSmallR+gridWidth,
            phi1=xxFGX phi2=90.-xxFGX,
            dz=frameDZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGXC
      BLOCK FGXC   C-grid, ray
      ATTRIBUTE FGXC seen=1 colo=3
      MATERIAL FR4
      SHAPE box dx=(diskOutR-diskInR-frameWidth)/2.,
            dy=gridWidth/2. dz=frameDZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGXD
      BLOCK FGXD   D-grid, ray
      ATTRIBUTE FGXD seen=1 colo=3
      MATERIAL FR4
      SHAPE box dx=(flatOutR-diskInR-frameWidth)/2.,
            dy=gridWidth/2. dz=frameDZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGWA
      BLOCK FGWA   A-elec, bar
      ATTRIBUTE FGWA seen=1 colo=1
      MATERIAL TermMix
      SHAPE box dx=0.3/2. dy=elecAdy/2. dz=2.7/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGWB
      BLOCK FGWB   APV board
      ATTRIBUTE FGWB seen=1 colo=1
      MATERIAL APVMix
      SHAPE box dx=25.92/2. dy=elecWidth/2. dz=4.04/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGWC
      BLOCK FGWC   interconnect board wider
      ATTRIBUTE FGWC seen=1 colo=1
      MATERIAL ConMix
      SHAPE box dx=0.3/2. dy=elecCdy/2. dz=3.15/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGWD
      BLOCK FGWD   gas feed connection
      ATTRIBUTE FGWD seen=1 colo=1
      MATERIAL Brass
      SHAPE tube rmin=0.2 rmax=0.5 dz=2.1/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGWE
      BLOCK FGWE   HV board
      ATTRIBUTE FGWE seen=1 colo=1
      MATERIAL HVMix
      SHAPE box dx=elecElen/2. dy=elecWidth/2.,
            dz=volElecDZ/2.
      ENDBLOCK
      <W> '===>> 	[End AgML Module FGTDGEO3]	 <<==='; (A32,/,/);
      END! Module FGTDGEO3
      MODULE IDSMGEO1    simplified  beam support cone for 2012
      Created   10/04/2011
      Author Jan Balewski MIT, Willie Leight MIT (material mixes)
      +CDE,agecom,gcunit,gconst.
      CONTENT IDSM,TPRR,TPRT,
      SUCA,  SUCB, SUCC, SUCD, SUCE, SUCF, SUCG,
      FGRL,FGHV
      REAL inR, outR,lengthZ,     k,sinA, cosA, resR, angRes,m,pm, j,angRail, rRail,
             dPhiHV, angFgtCbl

      STRUCTURE IDSC { version}
      STRUCTURE IDSG { version,rF,angFlat,r1Res,r2Res,rrRes,dangRes,dxRes,dyRes,fgtStartZ,
            fgtDiskStepZ,int fgtNdisk}
      STRUCTURE IDSA { version,x,y,z,thetax,thetay,thetaz,phix,phiy,phiz}
      FILL IDSC		! IDS control structure
      version = 1.0 ! Versioning of the IDSM geometry
      ENDFILL
      FILL IDSG		! IDS Geometry data,46.1cm=inner radii of IFC
      version = 1.0 ! 2012 Versionin of the IDS geometry
      rF = 2.25 !  radii of inner volume boundary
      angFlat = 106. !  angle (deg) for center of flat
      rrRes = 43. !  radial distance of  for TPC resistor tubes
      r1Res = 1.17 !  inner radii for TPC resistor tubes
      r2Res = 1.27 !  outer radii for TPC resistor tubes
      dangRes = 11.3 !  opening angle (deg) for TPC resistor tubes
      dxRes = 0.13 !  thicknessfor TPC resistor
      dyRes = 2. !  dy for TPC resistor
      fgtStartZ = 70. !  position of sensitive volume of the 1st disk
      fgtDiskStepZ = 10. !  disk separation along Z
      fgtNdisk = 6 ! number of disks
      ENDFILL
      FILL IDSG		! IDS Geometry data,46.1cm=inner radii of IFC
      version = 2.0 ! 2013 versionin of the IDS geometry
      rF = 2.25 !  radii of inner volume boundary
      angFlat = 106. !  angle (deg) for center of flat
      rrRes = 43. !  radial distance of  for TPC resistor tubes
      r1Res = 1.17 !  inner radii for TPC resistor tubes
      r2Res = 1.27 !  outer radii for TPC resistor tubes
      dangRes = 11.3 !  opening angle (deg) for TPC resistor tubes
      dxRes = 0.13 !  thicknessfor TPC resistor
      dyRes = 2. !  dy for TPC resistor
      fgtStartZ = 70. !  position of sensitive volume of the 1st disk
      fgtDiskStepZ = 10. !  disk separation along Z
      fgtNdisk = 6 ! number of disks
      ENDFILL
      FILL IDSA		! Alignment of the IDSM geometry within the CAVE no FGT cables
      version = 1.0 ! Default alignment of IDSM at (0,0,0) with no rotation
      x = 0.0 ! x-alignment
      y = 0.0 ! y-alignment
      z = 0.0 ! z-alignment
      thetax = 90.0 ! align x`-axis 90 degrees in theta wrt cave
      phix =  0.0 ! align x`-axis  0 degrees in phi   wrt cave
      thetay = 90.0 ! align y`-axis 90 degrees in theta wrt cave
      phiy = 90.0 ! align y`-axis  0 degrees in phi   wrt cave
      thetaz =  0.0 ! align z`-axis  0 degrees in theta wrt cave
      phiz =  0.0 ! align z`-axis  0 degrees in phi   wrt cave
      ENDFILL
      COMPONENT O A=16 Z=8 W=0.062
      COMPONENT C A=12 Z=6 W=0.892
      COMPONENT H A=1 Z=1 W=0.019
      COMPONENT Cl A=35.5 Z=17 W=0.027
      MIXTURE CFRPMix dens=1.78
      COMPONENT AL A=27 Z=13 W=1.
      MIXTURE AlPure dens=2.80
      COMPONENT O A=16 Z=8 W=0.043
      COMPONENT C A=12 Z=6 W=0.635
      COMPONENT H A=1 Z=1 W=0.014
      COMPONENT Cl A=35.5 Z=17 W=0.019
      COMPONENT AL A=27 Z=13 W=0.017
      COMPONENT Ti A=47.9 Z=22 W=0.260
      COMPONENT V A=50.9 Z=23 W=0.012
      MIXTURE SUCBMix dens=2.46
      COMPONENT O A=16 Z=8 W=0.048
      COMPONENT C A=12 Z=6 W=0.694
      COMPONENT H A=1 Z=1 W=0.015
      COMPONENT Cl A=35.5 Z=17 W=0.021
      COMPONENT AL A=27 Z=13 W=0.013
      COMPONENT Ti A=47.9 Z=22 W=0.200
      COMPONENT V A=50.9 Z=23 W=0.009
      MIXTURE SUCDMix dens=2.37
      COMPONENT O A=16 Z=8 W=0.032
      COMPONENT C A=12 Z=6 W=0.471
      COMPONENT H A=1 Z=1 W=0.011
      COMPONENT Cl A=35.5 Z=17 W=0.014
      COMPONENT AL A=27 Z=13 W=0.472
      MIXTURE RailMix dens=3.384
      COMPONENT Si A=28.08 Z=14 W=0.131
      COMPONENT O A=16 Z=8 W=0.117
      COMPONENT C A=12 Z=6 W=0.193
      COMPONENT H A=1 Z=1 W=0.024
      COMPONENT AL A=27 Z=13 W=0.143
      COMPONENT Cu A=63.5 Z=29 W=0.106
      COMPONENT F A=19. Z=9 W=0.254
      COMPONENT Na A=23. Z=11 W=0.015
      COMPONENT Ca A=40.1 Z=20 W=0.017
      MIXTURE CableMix dens=1.755
      COMPONENT AL A=27 Z=13 W=2.
      COMPONENT O A=16 Z=8 W=3.
      MIXTURE Alumina dens=3.90
      COMPONENT Si A=28.08 Z=14 W=0.281
      COMPONENT O A=16 Z=8 W=0.467
      COMPONENT C A=12 Z=6 W=0.220
      COMPONENT H A=1 Z=1 W=0.032
      MIXTURE FR4 dens=1.80
      use IDSC
      use IDSG version=IDSC_version
      use IDSA version=1.0
      <W> IDSC_version;  ('Inner Detector Support Module with IDSC_version = ', F5.2);
      inR     = IDSG_rF
      outR    = IDSG_rrRes + IDSG_r2Res
      lengthZ = 470.
      sinA = sin( IDSG_angflat * degrad )
      cosA = cos( IDSG_angflat * degrad )
      rRail=41.5
      dPhiHV=0.03
      CREATE IDSM
      POSITION IDSM x=IDSA_x y=IDSA_y z=IDSA_z thetax=IDSA_thetax phix=IDSA_phix  _
	thetay=IDSA_thetay phiy=IDSA_phiy thetaz=IDSA_thetaz phiz=IDSA_phiz 
      ! ---------------------------------------------------------------------------------- IDSM
      BLOCK IDSM   mother volume for beam support cone
      ATTRIBUTE IDSM seen=0 colo=4
      MATERIAL Air
      SHAPE tube rmin=inR rmax=outR dz=lengthZ/2.
      CREATE SUCA
      POSITION SUCA z=0 
      DO k = 0, 1
      pm=1.-2*k
      CREATE SUCB
      POSITION SUCB z=pm*55.35 
      IF ( k.eq.0 ) THEN
      CREATE SUCC
      POSITION SUCC z=59.55 
      ELSE
      POSITION SUCC z=-59.55 alphax=180. 
      ENDIF
      CREATE SUCD
      POSITION SUCD z=pm*63.41 
      CREATE SUCE
      POSITION SUCE z=pm*144.52 
      CREATE SUCF
      POSITION SUCF z=pm*224.52 
      CREATE SUCG
      POSITION SUCG z=pm*225.4 
      END DO
      DO m = 0, 1
      angRes = IDSG_angFlat - IDSG_dangRes/2.
      IF ( m.eq.1 ) THEN
      angRes = IDSG_angFlat+IDSG_dangRes/2.
      ENDIF
      CREATE TPRT
      POSITION TPRT x=IDSG_rrRes*cos(angRes/180.*3.1416)  _
	y=IDSG_rrRes*sin(angRes/180.*3.1416) z=0 
      END DO
      CREATE TPRR
      POSITION TPRR x=IDSG_rrRes*cosA y=IDSG_rrRes*sinA z=0 alphaz=IDSG_angFlat 
      DO m = 0, 1
      angRail=16./180.*3.1416
      IF ( m.eq.1 ) THEN
      angRail = angRail+3.1416
      ENDIF
      CREATE FGRL
      POSITION FGRL x=rRail*cos(angRail) y=rRail*sin(angRail) z=146.57 
      END DO
      DO m = 1, 4
      angFgtCbl=-90./180.*3.1416
      DO k = 1, IDSG_fgtnDisk
      CHECK (IDSG_version.eq.2.0).or.(k.eq.1).or.(m.le.3)
      pm=1
      IF ( (k.eq.1).or.(k.eq.3).or.(k.eq.5) ) THEN
      pm=-1
      ENDIF
      DO j = k, 16
      CREATE FGHV
      POSITION FGHV x=(rRail+0.5*pm)*cos(angFgtCbl+(dPhiHV*(m+2.2*k-pm*.8)))  _
	y=(rRail+0.5*pm)*sin(angFgtCbl+(dPhiHV*(m+2.2*k - pm*.8)))  _
	z=IDSG_fgtStartZ+IDSG_fgtDiskStepZ*(j-0.5) 
      END DO
      END DO
      END DO
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- SUCA
      BLOCK SUCA   central CFiber tube
      ATTRIBUTE SUCA seen=1 colo=6
      MATERIAL CFRPMix
      SHAPE tube rmin=21.5 rmax=21.6 dz=112./2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- SUCB
      BLOCK SUCB   small Alu ring at central tube
      ATTRIBUTE SUCB seen=1 colo=1
      MATERIAL SUCBMix
      SHAPE tube rmin=21.6 rmax=22.4 dz=1.3/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- SUCC
      BLOCK SUCC   CFiber cone section
      ATTRIBUTE SUCC seen=1 colo=6
      MATERIAL CFRPMix
      SHAPE cone dz=7.1/2. rmn1=21.6 rmx1=22.75,
            rmn2=38.75 rmx2=39.9
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- SUCD
      BLOCK SUCD   large Alu ring at cone
      ATTRIBUTE SUCD seen=1 colo=1
      MATERIAL SUCDMix
      SHAPE tube rmin=38.6 rmax=39.9 dz=0.62/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- SUCE
      BLOCK SUCE   East or West CFiber tube
      ATTRIBUTE SUCE seen=1 colo=6
      MATERIAL CFRPMix
      SHAPE tube rmin=39.8 rmax=39.9 dz=161.6/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- SUCF
      BLOCK SUCF   large Alu ring at the end of west cylinder
      ATTRIBUTE SUCF seen=1 colo=1
      MATERIAL AlPure
      SHAPE tube rmin=40. rmax=40.5 dz=1.6/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- SUCG
      BLOCK SUCG   large Alu end disk
      ATTRIBUTE SUCG seen=1 colo=1
      MATERIAL AlPure
      SHAPE tube rmin=10.3 rmax=40.5 dz=0.16/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- TPRR
      BLOCK TPRR    TPC resistor
      ATTRIBUTE TPRR seen=1 colo=2
      MATERIAL Alumina
      SHAPE box dx=IDSG_dxRes/2 dy=IDSG_dyRes/2.,
            dz=lengthZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- TPRT
      BLOCK TPRT   resistor protection,  carbon fiber
      ATTRIBUTE TPRT seen=1 colo=3
      MATERIAL FR4
      SHAPE tube rmin=IDSG_r1Res rmax=IDSG_r2Res,
            dz=lengthZ/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGRL
      BLOCK FGRL   FGT rail
      ATTRIBUTE FGRL seen=1 colo=2
      MATERIAL RailMix
      SHAPE tube rmin=0.9 rmax=1.0 dz=165.6/2.
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- FGHV
      BLOCK FGHV   FGT cables mixture
      ATTRIBUTE FGHV seen=1 colo=1
      MATERIAL CableMix
      SHAPE tube rmin=0. rmax=0.43 dz=IDSG_fgtDiskStepZ/2.
      ENDBLOCK
      <W> '===>> 	[End AgML Module IDSMGEO1]	 <<==='; (A32,/,/);
      END! Module IDSMGEO1
