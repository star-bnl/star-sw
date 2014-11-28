      SUBROUTINE STARLIGHTINTERFACE
      CHARACTER*1 BS1
      PARAMETER (BS1='\\')
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL STARLIGHTUSER

      CALL KUCMD(' ','starlight','C')
      GUID(1)='The STARLIGHT menu allows the user to specify the run-tim
     +e parameters'
      GUID(2)='of the STARLIGHT event generator.'
      CALL KUGUID('starlight',GUID,2,'S')

      CALL KUCMD('starlight',' ','SW')

      CALL KUCMD(' ','BEAM_1_Z','C')
      CALL KUPAR('BEAM_1_Z','z1','Z of beam 1','R','S')
      CALL KUPVAL('BEAM_1_Z','z1',0,1.0,' ','D')
      GUID(1)='Set the Z of beam 1'
      CALL KUGUID('BEAM_1_Z',GUID,1,'S')
      CALL KUACT('BEAM_1_Z',StarLightUser)

      CALL KUCMD(' ','BEAM_2_Z','C')
      CALL KUPAR('BEAM_2_Z','z2','Z of beam 2','R','S')
      CALL KUPVAL('BEAM_2_Z','z2',0,1.0,' ','D')
      GUID(1)='Set the Z of beam 2'
      CALL KUGUID('BEAM_2_Z',GUID,1,'S')
      CALL KUACT('BEAM_2_Z',StarLightUser)

      CALL KUCMD(' ','BEAM_1_A','C')
      CALL KUPAR('BEAM_1_A','a1','A of beam 1','R','S')
      CALL KUPVAL('BEAM_1_A','a1',0,1.0,' ','D')
      GUID(1)='Set the A of beam 1'
      CALL KUGUID('BEAM_1_A',GUID,1,'S')
      CALL KUACT('BEAM_1_A',StarLightUser)

      CALL KUCMD(' ','BEAM_2_A','C')
      CALL KUPAR('BEAM_2_A','a2','A of beam 2','R','S')
      CALL KUPVAL('BEAM_2_A','a2',0,1.0,' ','D')
      GUID(1)='Set the A of beam 2'
      CALL KUGUID('BEAM_2_A',GUID,1,'S')
      CALL KUACT('BEAM_2_A',StarLightUser)

      CALL KUCMD(' ','BEAM_GAMMA','C')
      CALL KUPAR('BEAM_GAMMA','gamma','Lorentz gamma of the beam','R',
     +'S')
      CALL KUPVAL('BEAM_GAMMA','gamma',0,1.0,' ','D')
      GUID(1)='Set the lorentz gamma of the beam'
      CALL KUGUID('BEAM_GAMMA',GUID,1,'S')
      CALL KUACT('BEAM_GAMMA',StarLightUser)

      CALL KUCMD(' ','W_MAX','C')
      CALL KUPAR('W_MAX','wmax','W_MAX','R','S')
      CALL KUPVAL('W_MAX','wmax',0,1.0,' ','D')
      GUID(1)='Set the W max'
      CALL KUGUID('W_MAX',GUID,1,'S')
      CALL KUACT('W_MAX',StarLightUser)

      CALL KUCMD(' ','W_MIN','C')
      CALL KUPAR('W_MIN','wmin','W_MIN','R','S')
      CALL KUPVAL('W_MIN','wmin',0,1.0,' ','D')
      GUID(1)='Set the W min'
      CALL KUGUID('W_MIN',GUID,1,'S')
      CALL KUACT('W_MIN',StarLightUser)

      CALL KUCMD(' ','W_N_BINS','C')
      CALL KUPAR('W_N_BINS','nbins','W_N_BINS','I','S')
      CALL KUPVAL('W_N_BINS','nbins',10,0.,' ','D')
      GUID(1)='Set the number of W bins'
      CALL KUGUID('W_N_BINS',GUID,1,'S')
      CALL KUACT('W_N_BINS',StarLightUser)

      CALL KUCMD(' ','RAP_MAX','C')
      CALL KUPAR('RAP_MAX','max','RAP_MAX','R','S')
      CALL KUPVAL('RAP_MAX','max',0,2.0,' ','D')
      GUID(1)='Set the max rapidity'
      CALL KUGUID('RAP_MAX',GUID,1,'S')
      CALL KUACT('RAP_MAX',StarLightUser)

      CALL KUCMD(' ','RAP_N_BINS','C')
      CALL KUPAR('RAP_N_BINS','nbins','RAP_N_BINS','I','S')
      CALL KUPVAL('RAP_N_BINS','nbins',10,0.,' ','D')
      GUID(1)='Set the number of rapidity bins'
      CALL KUGUID('RAP_N_BINS',GUID,1,'S')
      CALL KUACT('RAP_N_BINS',StarLightUser)

      CALL KUCMD(' ','CUT_PT','C')
      CALL KUPAR('CUT_PT','flag','PT_CUT','I','S')
      CALL KUPVAL('CUT_PT','flag',1,0.,' ','D')
      GUID(1)='Flag which enables the PT cut'
      CALL KUGUID('CUT_PT',GUID,1,'S')
      CALL KUACT('CUT_PT',StarLightUser)

      CALL KUCMD(' ','PT_MIN','C')
      CALL KUPAR('PT_MIN','ptmin','PT_MIN','R','S')
      CALL KUPVAL('PT_MIN','ptmin',0,1.0,' ','D')
      GUID(1)='Set the minimum PT'
      CALL KUGUID('PT_MIN',GUID,1,'S')
      CALL KUACT('PT_MIN',StarLightUser)

      CALL KUCMD(' ','PT_MAX','C')
      CALL KUPAR('PT_MAX','ptmax','PT_MAX','R','S')
      CALL KUPVAL('PT_MAX','ptmax',0,-1.0,' ','D')
      GUID(1)='Set the maximum PT'
      CALL KUGUID('PT_MAX',GUID,1,'S')
      CALL KUACT('PT_MAX',StarLightUser)

      CALL KUCMD(' ','CUT_ETA','C')
      CALL KUPAR('CUT_ETA','flag','ETA_CUT','I','S')
      CALL KUPVAL('CUT_ETA','flag',1,0.,' ','D')
      GUID(1)='Flag which enables the ETA cut'
      CALL KUGUID('CUT_ETA',GUID,1,'S')
      CALL KUACT('CUT_ETA',StarLightUser)

      CALL KUCMD(' ','ETA_MIN','C')
      CALL KUPAR('ETA_MIN','etamin','ETA_MIN','R','S')
      CALL KUPVAL('ETA_MIN','etamin',0,-2.0,' ','D')
      GUID(1)='Set the minimum eta'
      CALL KUGUID('ETA_MIN',GUID,1,'S')
      CALL KUACT('ETA_MIN',StarLightUser)

      CALL KUCMD(' ','ETA_MAX','C')
      CALL KUPAR('ETA_MAX','etamax','ETA_MAX','R','S')
      CALL KUPVAL('ETA_MAX','etamax',0,+2.0,' ','D')
      GUID(1)='Set the maximum eta'
      CALL KUGUID('ETA_MAX',GUID,1,'S')
      CALL KUACT('ETA_MAX',StarLightUser)

      CALL KUCMD(' ','PROD_MODE','C')
      CALL KUPAR('PROD_MODE','prod','PROD_MODE','I','S')
      CALL KUPVAL('PROD_MODE','prod',1,0.,' ','D')
      GUID(1)='Set the production mode'
      CALL KUGUID('PROD_MODE',GUID,1,'S')
      CALL KUACT('PROD_MODE',StarLightUser)

      CALL KUCMD(' ','PROD_PID','C')
      CALL KUPAR('PROD_PID','pid','PROD_PID','I','S')
      CALL KUPVAL('PROD_PID','pid',121,0.,' ','D')
      GUID(1)='Set the production PID'
      CALL KUGUID('PROD_PID',GUID,1,'S')
      CALL KUACT('PROD_PID',StarLightUser)

      CALL KUCMD(' ','OUTPUT_FORMAT','C')
      CALL KUPAR('OUTPUT_FORMAT','fmt','OUTPUT_FORMAT','I','S')
      CALL KUPVAL('OUTPUT_FORMAT','fmt',1,0.,' ','D')
      GUID(1)='Set the output format'
      CALL KUGUID('OUTPUT_FORMAT',GUID,1,'S')
      CALL KUACT('OUTPUT_FORMAT',StarLightUser)

      CALL KUCMD(' ','BREAKUP_MODE','C')
      CALL KUPAR('BREAKUP_MODE','mode','BREAKUP_MODE','I','S')
      CALL KUPVAL('BREAKUP_MODE','mode',1,0.,' ','D')
      GUID(1)='Set the breakup mode'
      CALL KUGUID('BREAKUP_MODE',GUID,1,'S')
      CALL KUACT('BREAKUP_MODE',StarLightUser)

      CALL KUCMD(' ','INTERFERENCE','C')
      CALL KUPAR('INTERFERENCE','flag','INTERFERENCE','I','S')
      CALL KUPVAL('INTERFERENCE','flag',1,0.,' ','D')
      GUID(1)='Set the interference'
      CALL KUGUID('INTERFERENCE',GUID,1,'S')
      CALL KUACT('INTERFERENCE',StarLightUser)

      CALL KUCMD(' ','IF_STRENGTH','C')
      CALL KUPAR('IF_STRENGTH','strength','IF_STRENGTH','R','S')
      CALL KUPVAL('IF_STRENGTH','strength',0,1.0,' ','D')
      GUID(1)='Set the interference strength'
      CALL KUGUID('IF_STRENGTH',GUID,1,'S')
      CALL KUACT('IF_STRENGTH',StarLightUser)

      CALL KUCMD(' ','COHERENT','C')
      CALL KUPAR('COHERENT','flag','CONHERENT','I','S')
      CALL KUPVAL('COHERENT','flag',1,0.,' ','D')
      GUID(1)='Set the coherence'
      CALL KUGUID('COHERENT',GUID,1,'S')
      CALL KUACT('COHERENT',StarLightUser)

      CALL KUCMD(' ','INCO_FACTOR','C')
      CALL KUPAR('INCO_FACTOR','fact','INCO_FACTOR','R','S')
      CALL KUPVAL('INCO_FACTOR','fact',0,1.0,' ','D')
      GUID(1)='Set the incoherence factor'
      CALL KUGUID('INCO_FACTOR',GUID,1,'S')
      CALL KUACT('INCO_FACTOR',StarLightUser)

      CALL KUCMD(' ','BFORD','C')
      CALL KUPAR('BFORD','slope','BFORD','R','S')
      CALL KUPVAL('BFORD','slope',0,0.0,' ','D')
      GUID(1)='Set the deuteron slope parameter'
      CALL KUGUID('BFORD',GUID,1,'S')
      CALL KUACT('BFORD',StarLightUser)

      CALL KUCMD(' ','INT_PT_MAX','C')
      CALL KUPAR('INT_PT_MAX','max','INT_PT_MAX','R','S')
      CALL KUPVAL('INT_PT_MAX','max',0,10.0,' ','D')
      GUID(1)='Set the maximum pt interference INT_PT_MAX'
      CALL KUGUID('INT_PT_MAX',GUID,1,'S')
      CALL KUACT('INT_PT_MAX',StarLightUser)

      CALL KUCMD(' ','INT_PT_N_BINS','C')
      CALL KUPAR('INT_PT_N_BINS','nbins','INT_PT_N_BINS','I','S')
      CALL KUPVAL('INT_PT_N_BINS','nbins',10,0.,' ','D')
      GUID(1)='Set the number of bins for pt interference INT_PT_N_BINS'
      CALL KUGUID('INT_PT_N_BINS',GUID,1,'S')
      CALL KUACT('INT_PT_N_BINS',StarLightUser)

      CALL KUCMD('..',' ','SW')

      CALL KUCMD('/',' ','SW')

      END
