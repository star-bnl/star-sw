      SUBROUTINE BPYTHIA
      CHARACTER*1 BS1
      PARAMETER (BS1='\\')
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL APYTUSER

      CALL KUCMD(' ','PYTHIA','C')
      GUID(1)='The PYTHIA menu allows to read the old-fashion FFREAD'
      GUID(2)='parameter input for the new (double precision) PYTHIA'
      GUID(3)='versions 6.1 and 6.2.'
      CALL KUGUID('PYTHIA',GUID,3,'S')

      CALL KUCMD('PYTHIA',' ','SW')

      CALL KUCMD(' ','FRAME','C')
      CALL KUPAR('FRAME','frame','collision frame','C','S')
      CALL KUPVAL('FRAME','frame',0,0.,'CMS','D')
      GUID(1)='Select the collision frame - Center Of Mass, Fix Target,'
     +//' etc'
      GUID(2)='In the case of CMS frame, one can set the CMS energy usin
     +g'
      GUID(3)='the ENER command.  Otherwise, set beam and target momenta
     +'
      GUID(4)='using pblue and pyell.'
      CALL KUGUID('FRAME',GUID,4,'S')
      CALL KUACT('FRAME',apytuser)

      CALL KUCMD(' ','BEAM','C')
      CALL KUPAR('BEAM','beam','composition of the incident beam','C',
     +'S')
      CALL KUPVAL('BEAM','beam',0,0.,'P','D')
      CALL KUPAR('BEAM','target','composition of the target (beam)','C',
     +'S')
      CALL KUPVAL('BEAM','target',0,0.,'P','D')
      GUID(1)='Describes the composition of the collision - PP, ee, etc.
     +'
      GUID(2)='Character variables to specify beam and target particles.
     +'
      GUID(3)='Upper-case and lower-case letters may be freely mixed'
      CALL KUGUID('BEAM',GUID,3,'S')
      CALL KUACT('BEAM',apytuser)

      CALL KUCMD(' ','PBLUE','C')
      CALL KUPAR('PBLUE','px','x-component in GeV','R','S')
      CALL KUPVAL('PBLUE','px',0,0.0,' ','D')
      CALL KUPAR('PBLUE','py','y-component in GeV','R','S')
      CALL KUPVAL('PBLUE','py',0,0.0,' ','D')
      CALL KUPAR('PBLUE','pz','z-component in GeV','R','S')
      CALL KUPVAL('PBLUE','pz',0,250.0,' ','D')
      GUID(1)='Provides the blue beam (+z) three-momentum.'
      CALL KUGUID('PBLUE',GUID,1,'S')
      CALL KUACT('PBLUE',apytuser)

      CALL KUCMD(' ','PYELL','C')
      CALL KUPAR('PYELL','px','x-component in GeV','R','S')
      CALL KUPVAL('PYELL','px',0,0.0,' ','D')
      CALL KUPAR('PYELL','py','y-component in GeV','R','S')
      CALL KUPVAL('PYELL','py',0,0.0,' ','D')
      CALL KUPAR('PYELL','pz','z-component in GeV','R','S')
      CALL KUPVAL('PYELL','pz',0,-250.0,' ','D')
      GUID(1)='Provides the yellow beam (-z) three-momentum.'
      CALL KUGUID('PYELL',GUID,1,'S')
      CALL KUACT('PYELL',apytuser)

      CALL KUCMD(' ','ENER','C')
      CALL KUPAR('ENER','energy','collision energy','R','S')
      CALL KUPVAL('ENER','energy',0,14000.,' ','D')
      GUID(1)='Select the collision energy in the current frame.'
      GUID(2)='Default is LHC nominal energy - 7 + 7 Tev.'
      CALL KUGUID('ENER',GUID,2,'S')
      CALL KUACT('ENER',apytuser)

      CALL KUCMD(' ','MSEL','C')
      CALL KUPAR('MSEL','msel','hard scattering subprocess selector',
     +'I','S')
      CALL KUPVAL('MSEL','msel',1,0.,' ','D')
      GUID(1)='Selection of hard scattering subprocesses'
      CALL KUGUID('MSEL',GUID,1,'S')
      CALL KUACT('MSEL',apytuser)

      CALL KUCMD(' ','TUNE','C')
      CALL KUPAR('TUNE','tune','index of the pythia tune','I','S')
      CALL KUPVAL('TUNE','tune',100,0.,' ','D')
      GUID(1)='Selection of the pythia tune.  Default corresponds to CDF
     + tune A.'
      GUID(2)='Consult the pythia manual (or comments in source code of'
     +//' pytune.F)'
      GUID(3)='for the available options.'
      GUID(4)='For pythia 6.3 and earlier:'
      GUID(5)='  http://home.thep.lu.se/~torbjorn/Pythia.html'
      GUID(6)='For pythia 6.4:'
      GUID(7)='  http://projects.hepforge.org/pythia6/'
      GUID(8)='Example:'
      GUID(9)='  TUNE 100'
      CALL KUGUID('TUNE',GUID,9,'S')
      CALL KUACT('TUNE',apytuser)

      CALL KUCMD(' ','PMAS','C')
      CALL KUPAR('PMAS','pmas',
     +'given particle code and selector set mass value','C','S')
      GUID(1)='provide a mass value for a particle defined with its inde
     +x'
      CALL KUGUID('PMAS',GUID,1,'S')
      CALL KUACT('PMAS',apytuser)

      CALL KUCMD(' ','PMA1','C')
      CALL KUPAR('PMA1','index_mass','index and mass value','C','S')
      GUID(1)='provide a mass value for a particle defined with its inde
     +x'
      CALL KUGUID('PMA1',GUID,1,'S')
      CALL KUACT('PMA1',apytuser)

      CALL KUCMD(' ','PMA2','C')
      CALL KUPAR('PMA2','index_mass','index and mass value','C','S')
      GUID(1)='provide a mass value for a particle defined with its inde
     +x'
      CALL KUGUID('PMA2',GUID,1,'S')
      CALL KUACT('PMA2',apytuser)

      CALL KUCMD(' ','PMA3','C')
      CALL KUPAR('PMA3','index_mass','index and mass value','C','S')
      GUID(1)='provide a mass value for a particle defined with its inde
     +x'
      CALL KUGUID('PMA3',GUID,1,'S')
      CALL KUACT('PMA3',apytuser)

      CALL KUCMD(' ','PMA4','C')
      CALL KUPAR('PMA4','index_mass','index and mass value','C','S')
      GUID(1)='provide a mass value for a particle defined with its inde
     +x'
      CALL KUGUID('PMA4',GUID,1,'S')
      CALL KUACT('PMA4',apytuser)

      CALL KUCMD(' ','MDME1','C')
      CALL KUPAR('MDME1','decay_modes','index and decay mode value','C',
     +'S')
      GUID(1)='provide a decay mode value for a particle defined with it
     +s index'
      CALL KUGUID('MDME1',GUID,1,'S')
      CALL KUACT('MDME1',apytuser)

      CALL KUCMD(' ','CKIN','C')
      CALL KUPAR('CKIN','ckin','hard scattering parameters 1-200','C',
     +'S')
      GUID(1)='Hard scattering kinematics cuts like Emax, Pt.'
      GUID(2)='Most important of these are CKIN(1)-CKIN(8), CKIN(27)-CKI
     +N(28)'
      CALL KUGUID('CKIN',GUID,2,'S')
      CALL KUACT('CKIN',apytuser)

      CALL KUCMD(' ','MRPY','C')
      CALL KUPAR('MRPY','mpry','key 1-6','C','S')
      GUID(1)='Random number setting for Pythia internal random'
      GUID(2)='number generator. If not set, the built-in starsim'
      GUID(3)='generator will be used instead.'
      CALL KUGUID('MRPY',GUID,3,'S')
      CALL KUACT('MRPY',apytuser)

      CALL KUCMD(' ','MSUB','C')
      CALL KUPAR('MSUB','msub','Process key 1-500','C','S')
      GUID(1)='Switch on a selected process.'
      CALL KUGUID('MSUB',GUID,1,'S')
      CALL KUACT('MSUB',apytuser)

      CALL KUCMD(' ','MSTP','C')
      CALL KUPAR('MSTP','mstp','Pythia parameters 1-200','C','S')
      GUID(1)='Pythia parameters:'
      GUID(2)=' MSTP(1) - maximum number of generations.'
      GUID(3)=' MSTP(2) - calculation of alpha-s at hard interaction.'
      GUID(4)=' MSTP(3) - selection of Lambda value in alpha-s.'
      GUID(5)=' MSTP(4) - treatment of the Higgs sector.'
      GUID(6)=' MSTP(5) - presence of anomalous couplings in processes.'
      GUID(7)=' MSTP(7) - choice of heavy flavour in subprocesses.'
      CALL KUGUID('MSTP',GUID,7,'S')
      CALL KUACT('MSTP',apytuser)

      CALL KUCMD(' ','PARP','C')
      CALL KUPAR('PARP','msub','hard scattering paramters 1-200','C',
     +'S')
      GUID(1)='some of the parameters are :'
      GUID(2)=' PARP(1) - used in running Alpha_s for hard scattering'
      GUID(3)=' PARP(2) - lowest energy for the event as a whole that'
      GUID(4)='the program will accept to simulate'
      GUID(5)=' PARP(13) - maximum scale allowed for photoproduction whe
     +n using the option'
      GUID(6)=' PARP(14) - in the numerical integration of quark and glu
     +on parton'
      GUID(7)='distributions inside an electron, the successive halvings
     + of evaluation'
      GUID(8)='point spacing is interrupted when two values agree in rel
     +ative size.'
      CALL KUGUID('PARP',GUID,8,'S')
      CALL KUACT('PARP',apytuser)

      CALL KUCMD(' ','MSTU','C')
      CALL KUPAR('MSTU','mstu','just paramters 1-200','C','S')
      GUID(1)=' MSTU(1)-MSTU(3) Variables used by the event study routin
     +es'
      GUID(2)=' MSTU(4) number of lines available in the common block PY
     +JETS'
      GUID(3)=' MSTU(5) is used in building up the special colour-flow i
     +nformation'
      CALL KUGUID('MSTU',GUID,3,'S')
      CALL KUACT('MSTU',apytuser)

      CALL KUCMD(' ','PARU','C')
      CALL KUPAR('PARU','msub','just paramters 1-200','C','S')
      GUID(1)=' PARU(1)  - pi approx 3.141592653589793'
      GUID(2)=' PARU(2)  - 2pi appro 6.283185307179586'
      GUID(3)=' PARU(3)  - conversion factor for 1/GeV to fm'
      GUID(4)=' PARU(4)  - conversion factor for fm 1/GeV'
      GUID(5)=' PARU(5)  - conversion factor for 1/GeV^2 to mb'
      GUID(6)=' PARU(6)  - conversion factor for mb to 1/GeV^2'
      GUID(7)=' PARU(11) - relative error'
      GUID(8)=' PARU(12) - effective cut-off in squared mass'
      GUID(9)=' PARU(13) - effective angular cut-off in radians for reco
     +mbination of partons'
      CALL KUGUID('PARU',GUID,9,'S')
      CALL KUACT('PARU',apytuser)

      CALL KUCMD(' ','MSTJ','C')
      CALL KUPAR('MSTJ','msub','just paramters 1-200','C','S')
      GUID(1)='some useful parameters are :'
      GUID(2)=' MSTJ(1) - choice of fragmentation scheme'
      GUID(3)=' MSTJ(2) - gluon jet fragmentation scheme in independent'
     +//' fragmentation'
      GUID(4)=' MSTJ(3) - energy, momentum and flavour conservation opti
     +ons in independent fragmentation'
      GUID(5)=' MSTJ(11) - choice of longitudinal fragmentation function
     +'
      GUID(6)=' MSTJ(12) - choice of baryon production model'
      GUID(7)=' MSTJ(13) - generation of transverse momentum for endpoin
     +t quark(s)'
      GUID(8)=' MSTJ(14) - treatment of a colour-singlet parton system w
     +ith a low invariant mass'
      CALL KUGUID('MSTJ',GUID,8,'S')
      CALL KUACT('MSTJ',apytuser)

      CALL KUCMD(' ','IMSS','C')
      CALL KUPAR('IMSS','msub','SUSY Treatment 1-200','C','S')
      GUID(1)='SUSY treatment key (page 199) :'
      GUID(2)=' IMSS(1) level of MSSM simulation'
      GUID(3)=' IMSS(2) treatment of U,SU and SU(3) gaugino mass paramet
     +ers'
      GUID(4)=' IMSS(3) treatment of the gluino mass parameter'
      GUID(5)=' IMSS(4) treatment of the Higgs sector'
      GUID(6)=' IMSS(5) allows you to set the st sbo and stau masses and
     + mixing by hand'
      CALL KUGUID('IMSS',GUID,6,'S')
      CALL KUACT('IMSS',apytuser)

      CALL KUCMD(' ','RMSS','C')
      CALL KUPAR('RMSS','msub','SUSY Parameters 1-200','C','S')
      GUID(1)='SUSY Parameters (page 200):'
      GUID(2)=' RMSS(1) - is the common gaugino mass'
      GUID(3)=' RMSS(4) - fixes the sign of the higgsino mass'
      GUID(4)=' RMSS(5) - the ratio of Higgs expectation'
      GUID(5)=' RMSS(2) - gaugino mass'
      GUID(6)=' RMSS(6) - Left slepton mass'
      GUID(7)=' RMSS(7) - Right slepton mass'
      GUID(8)=' RMSS(8) - Left squark mass'
      GUID(9)=' RMSS(9) - Right squark mass'
      CALL KUGUID('RMSS',GUID,9,'S')
      CALL KUACT('RMSS',apytuser)

      CALL KUCMD(' ','PARJ','C')
      CALL KUPAR('PARJ','msub','suppresion paramters 1-200','C','S')
      GUID(1)='some useful parameters are:'
      GUID(2)=' PARJ(1) the suppression of diquark-antidiquark pair'
      GUID(3)='production in the colour field.'
      GUID(4)=' PARJ(2) the suppression of s quark pair production in'
      GUID(5)='the field compared with u or d pair production.'
      GUID(6)=' PARJ(3)  - PARJ(10) the suppression of quarks'
      GUID(7)=' PARJ(11) - PARJ(17) spin of mesons'
      CALL KUGUID('PARJ',GUID,7,'S')
      CALL KUACT('PARJ',apytuser)

      CALL KUCMD(' ','MDCY','C')
      CALL KUPAR('MDCY','mdcy',
     +'compressed particle code (KC), index and decay value','C','S')
      GUID(1)='MDCY(KC,1) switch on/off the decay of particle defined by
     + its'
      GUID(2)='compressed code'
      GUID(3)='MDCY(KC,2) gives the entry point into the decay channel t
     +able'
      GUID(4)='for compressed particle code KC'
      GUID(5)='MDCY(KC,3) gives the total number of decay channels defin
     +ed'
      GUID(6)='for compressed particle code KC'
      GUID(7)=' Example:  MDCY (25,1) = 5'
      GUID(8)=' or:       MDCY1 25=5'
      GUID(9)=' (courtesy of Mercedes Paniccia).'
      CALL KUGUID('MDCY',GUID,9,'S')
      CALL KUACT('MDCY',apytuser)

      CALL KUCMD(' ','MDCY1','C')
      CALL KUPAR('MDCY1','mdcy1',
     +'particle code and decay on/off switch','C','S')
      GUID(1)='MDCY(KC,1) switch on/off the decay of particle defined'
      GUID(2)='by its compressed code, example MDCY1 25 = 5.'
      CALL KUGUID('MDCY1',GUID,2,'S')
      CALL KUACT('MDCY1',apytuser)

      CALL KUCMD(' ','MDCY2','C')
      CALL KUPAR('MDCY2','mdcy2',
     +'particle code and entry point into the decay channel table','C',
     +'S')
      GUID(1)='MDCY(KC,2) gives the entry point into the decay channel'
      GUID(2)='table for compressed particle code KC'
      CALL KUGUID('MDCY2',GUID,2,'S')
      CALL KUACT('MDCY2',apytuser)

      CALL KUCMD(' ','MDCY3','C')
      CALL KUPAR('MDCY3','mdcy3',
     +'particle code and total number of decay channels for the particle
     +','C','S')
      GUID(1)='MDCY(KC,3) gives the total number of decay channels defin
     +ed for'
      GUID(2)='compressed particle code KC'
      CALL KUGUID('MDCY3',GUID,2,'S')
      CALL KUACT('MDCY3',apytuser)

      CALL KUCMD(' ','KFPR','C')
      CALL KUPAR('KFPR','kfpr',
     +'set flavours for products of a given subprocess','C','S')
      GUID(1)='given subprocess code (ISUB:1-200) and product index (J:1
     +-2)'
      GUID(2)='set product flavour (KF)'
      GUID(3)=' example:  KFPR (121,1) = 25.'
      GUID(4)=' or        KFPR1 121 = 25.'
      GUID(5)=' (courtesy of Mercedes Paniccia).'
      CALL KUGUID('KFPR',GUID,5,'S')
      CALL KUACT('KFPR',apytuser)

      CALL KUCMD(' ','KFPR1','C')
      CALL KUPAR('KFPR1','kfpr1',
     +'first product subprocess code and product flavour','C','S')
      GUID(1)='set flavours(KF) for products of a given subprocess(1-200
     +),'
      GUID(2)='example:  KFPR1 121 = 25.'
      CALL KUGUID('KFPR1',GUID,2,'S')
      CALL KUACT('KFPR1',apytuser)

      CALL KUCMD(' ','KFPR2','C')
      CALL KUPAR('KFPR2','kfpr2',
     +'second product subprocess code and product flavour','C','S')
      GUID(1)='set flavours(KF) for products of a given subprocess(1-200
     +),'
      GUID(2)='example:  KFPR2 121 = 5'
      GUID(3)='(courtesy of Mercedes Paniccia).'
      CALL KUGUID('KFPR2',GUID,3,'S')
      CALL KUACT('KFPR2',apytuser)

      CALL KUCMD('..',' ','SW')

      CALL KUCMD('/',' ','SW')

      END
