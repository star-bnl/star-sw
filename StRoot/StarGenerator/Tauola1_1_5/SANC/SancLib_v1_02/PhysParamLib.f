       DOUBLE PRECISION FUNCTION getMass(Ptype)
c###############################################
c                                             ##
c         Masses of Particles in GeV          ##
c                                             ##
c###############################################
         IMPLICIT NONE
         INTEGER          Ptype
            
         SELECT CASE(Ptype)
            CASE( 0)                                       !________ B O S O N S
                   getMass = 0d0             ! Gamma         
            CASE(-1) 
                   getMass = 91.1867d0       ! Z    
            CASE(-2) 
                   getMass = 120d0           ! H    
            CASE(-3)        
                   getMass = 80.4514958d0    ! W^{+}    
            CASE(-4)       
                   getMass = 80.4514958d0    ! W^{-} 
                                                           !________ L E P T O N T S    
            CASE( 1)                        
                   getMass = 0d0             ! neutrino_electron
            CASE( 2)         
                   getMass = 0.51099907d-3   ! electron
            CASE( 3)         
                   getMass = 0d0             ! neutrino_muon
            CASE( 4)        
                   getMass = 105.658389d-3   ! miuon
            CASE( 5)         
                   getMass = 0d0             ! neutrino_tau
            CASE( 6)         
                   getMass = 1.77705d0       ! tau

            CASE( 7)                                       !________ Q U A R K S        
                   getMass = 62.0d-3         ! up
            CASE( 8)          
                   getMass = 83.0d-3         ! down 
            CASE( 9)         
                   getMass = 1.5d0           ! c   
            CASE(10)         
                   getMass = 215.0d-3        ! s   
            CASE(11)         
                   getMass = 173.8d0         ! t   
            CASE(12)        
                   getMass = 4.7d0           ! b   
            CASE default
               CALL MessageRoutine(1,.true.,"getMass")
               STOP          
         END SELECT

       RETURN
       END           


       DOUBLE PRECISION FUNCTION getCharge(Ptype)
c############################################################
c                                                          ##  
c   Charges for Particles (in the electron charge unit!!)  ##
c                                                          ## 
c############################################################
         IMPLICIT NONE 
         INTEGER          Ptype

         SELECT CASE(Ptype)
            CASE( 0)         ! Gamma                       !________ B O S O N S
                   getCharge =  0d0
            CASE(-1)      
                   getCharge =  0d0         ! Z     
            CASE(-2)             
                   getCharge =  0d0         ! H    
            CASE(-3)         
                   getCharge = +1d0         ! W^{+}    
            CASE(-4)         
                   getCharge = -1d0         ! W^{-}    
                                                           !________ L E P T O N T S    
            CASE( 1)          
                   getCharge =  0d0         ! neutrino_electron
            CASE( 2)          
                   getCharge = -1d0         ! electron
            CASE( 3)          
                   getCharge =  0d0         ! neutrino_muon
            CASE( 4)          
                   getCharge = -1d0         ! miuon
            CASE( 5)          
                   getCharge =  0d0         ! neutrino_tau
            CASE( 6)          
                   getCharge = -1d0         ! tau                
                                                            !________ Q U A R K S        
            CASE( 7)             
                   getCharge = +2d0/3d0     ! up                
            CASE( 8)             
                   getCharge = -1d0/3d0     ! down 
            CASE( 9)              
                   getCharge = +2d0/3d0     ! c
            CASE(10)             
                   getCharge = -1d0/3d0     ! s
            CASE(11)             
                   getCharge = +2d0/3d0     ! t
            CASE(12)             
                   getCharge = -1d0/3d0     ! b         
            CASE default
               CALL MessageRoutine(1,.true.,"getCharge")
               STOP          
         END SELECT

       RETURN
       END


       DOUBLE PRECISION FUNCTION getWIS(Ptype)
c##################################################
c                                                ##
c   Weak IsoSpin (I3i) of Leptons and Quarks ... ##
c                                                ## 
c##################################################
         IMPLICIT NONE
         INTEGER          Ptype

         SELECT CASE(Ptype)
                                                            !________ L E P T O N T S    
            CASE( 1)           
                   getWIS = +1d0/2d0      ! neutrino_electron                          
            CASE( 2)           
                   getWIS = -1d0/2d0      ! electron
            CASE( 3)           
                   getWIS = +1d0/2d0      ! neutrino_muon
            CASE( 4)           
                   getWIS = -1d0/2d0      ! miuon
            CASE( 5)           
                   getWIS = +1d0/2d0      ! neutrino_tau
            CASE( 6)           
                   getWIS = -1d0/2d0       ! tau              
                                                            !________ Q U A R K S        
            CASE( 7)           
                   getWIS = +1d0/2d0      ! up   
            CASE( 8)           
                   getWIS = -1d0/2d0      ! down 
            CASE( 9)           
                   getWIS = +1d0/2d0      ! c   
            CASE(10)          
                   getWIS = -1d0/2d0      ! s   
            CASE(11)          
                   getWIS = +1d0/2d0      ! t   
            CASE(12)          
                   getWIS = -1d0/2d0      ! b   
            CASE default
               CALL MessageRoutine(1,.true.,"getWIS")
               STOP          
         END SELECT

       RETURN
       END

       SUBROUTINE ConvertPNS(upPart,dnPart,s2nIn)
c######################################################
c    Converts Particle Numbering Scheme from S2N to   #
c    Monte Carlo                                      #
c                                                     #
c    Input : upPart, dnPart                           #
c    Output: s2nIn                                    #
c    s2nIn is a channel identification in s2n !!!     #
c######################################################
         IMPLICIT NONE
         INTEGER s2nIn,sxv1,sxv2,upPart,dnPart,dcPart
         INCLUDE "MC_Declare.h"   

         sxv1 = dnPart-upPart
         sxv2 = dnPart-upPart+1-2*mod(upPart,2)          ! check of doublets...
         IF (sxv1.NE.0 .AND. (decPart.EQ.-1 .OR. decPart.EQ.-2)) THEN
           CALL MessageRoutine(1,.true.,"ConvertPNS")
           STOP
         ENDIF
         IF (sxv2.NE.0 .AND. (decPart.EQ.-3 .OR. decPart.EQ.-4)) THEN
           CALL MessageRoutine(1,.true.,"ConvertPNS")
           STOP
         ENDIF
         IF ((upPart.GE.7.AND.upPart.LE.12) .AND. (dnPart.GE.7.AND.dnPart.LE.12)) THEN 
            s2nIn = Min(upPart,dnPart)-3 
         ELSEIF ((upPart.GE.1.AND.upPart.LE.6) .AND. (dnPart.GE.1.AND.dnPart.LE.6)) THEN 
            s2nIn = Max(upPart,dnPart)/2 
         ELSE
            CALL MessageRoutine(1,.true.,"CoonvertPNS")
            STOP          
         ENDIF

       RETURN
       END

      SUBROUTINE  MessageRoutine(type,error,procedureName)
                    
        IMPLICIT NONE
        CHARACTER procedureName*(*),genType*30,softEvent*3,hardEvent*3
        LOGICAL   error
        INTEGER   type
        INCLUDE   "MC_Declare.h"    
                    
        IF (type.EQ.1) THEN       ! notation of particles ....

           WRITE(mcLUN,*)""
           WRITE(mcLUN,*)"     ERROR IN  ",procedureName," -- USE CORRECT VALUE FOR `Ptype':"
           WRITE(mcLUN,*)""
              
           IF (error.AND.type.EQ.1) THEN
             WRITE(mcLUN,*)""
             WRITE(mcLUN,'(3X,A65)') "#################__PARTICLE__NUMBERING__SCHEME__#################"
             WRITE(mcLUN,'(3X,A65)') "#                                                               #"
             WRITE(mcLUN,'(3X,A65)') "# _B_O_S_O_N_S_:                                                #"  
             WRITE(mcLUN,'(3X,A65)') "#   gamma = 0  Z = -1  H = -2  W^{+} = -3  W^{-} = -4           #"  
             WRITE(mcLUN,'(3X,A65)') "#                                                               #"
             WRITE(mcLUN,'(3X,A65)') "# _L_E_P_T_O_N_S_:                                              #"
             WRITE(mcLUN,'(3X,A65)') "#   neutrino_electron = 1  neutrino_muon = 3  neutrino_tau = 5  #" 
             WRITE(mcLUN,'(3X,A65)') "#   electron          = 2  muon          = 4  tau          = 6  #" 
             WRITE(mcLUN,'(3X,A65)') "#                                                               #"
             WRITE(mcLUN,'(3X,A65)') "# _Q_U_A_R_K_S_:                                                #"
             WRITE(mcLUN,'(3X,A65)') "#   up   = 7  c = 9   t = 11                                    #"
             WRITE(mcLUN,'(3X,A65)') "#   down = 8  s = 10  b = 12                                    #"
             WRITE(mcLUN,'(3X,A65)') "#################################################################"
             WRITE(mcLUN,'(3X,A65)')""
           ENDIF
              
        ELSEIF (type.EQ.2.AND.error) THEN           ! random number generator..
           IF (ranGenType .EQ. 1) THEN
              genType = "__RANLUX__"           
           ELSEIF (ranGenType .EQ. 2) THEN
              genType = "__RANMAR__"           
           ELSEIF (ranGenType .EQ. 3) THEN
              genType = "__MERSENNE TWISTER__"           
           ELSE 
              WRITE(mcLUN,*)""
              WRITE(mcLUN,*)"  ERROR IN  ",procedureName,"  :"
              WRITE(mcLUN,*)"        NO RANDOM GNERATOR IS INITIALIZED:" 
              WRITE(mcLUN,*)"            RanGenType = 1 for 'ranlux'"    
              WRITE(mcLUN,*)"            RanGenType = 2 for 'ranmar'"   
              WRITE(mcLUN,*)"            RanGenType = 3 for 'mersenne twister'"   
              WRITE(mcLUN,*)""
              STOP
           ENDIF
           WRITE(mcLUN,*) ""
           WRITE(mcLUN,'(3X,A42)') "##########################################"
           WRITE(mcLUN,'(3X,A42)') "#                                        #"
           WRITE(mcLUN,'(3X,A42)') "# USED RANDOM NUMBER GENERATOR :         #"
           WRITE(mcLUN,'(3X,A42)') "#                                        #" 
           WRITE(mcLUN,'(3X,A10,A30,A2)') "#         ",genType,"#"
           WRITE(mcLUN,'(3X,A42)') "##########################################"
           WRITE(mcLUN,*) ""

        ELSEIF (type.EQ.3.AND.error) THEN           ! Enevt type messeger ...
           IF(.NOT.softevnt.AND..NOT.hardEvnt) THEN
             WRITE(mcLUN,*) ""
             WRITE(mcLUN,*) "ERROR IN ",procedureName,"   :"
             WRITE(mcLUN,*) "      NO EVENTS WILL BE GENERATED :"
             WRITE(mcLUN,*) "             Hard Event =",hardEvnt
             WRITE(mcLUN,*) "             Soft Event =",softEvnt
             WRITE(mcLUN,*) ""
             STOP 
           ENDIF
           IF (hardEvnt)      hardEvent="ON"
           IF (.NOT.hardEvnt) hardEvent="OFF"
           IF (softEvnt)      softEvent="ON"
           IF (.NOT.softEvnt) softEvent="OFF"
           WRITE(mcLUN,*) ""
           WRITE(mcLUN,*) "MESSAGE FROM ",procedureName,"   :"
           WRITE(mcLUN,*) "        TYPE OF GENERATED EVENTS :"
           WRITE(mcLUN,*) "               Hard Event = ",hardEvent
           WRITE(mcLUN,*) "               Soft Event = ",softEvent
           WRITE(mcLUN,*) ""
 
        ELSEIF (type.EQ.4.AND.error) THEN           ! Check event weight sign on negativeness !!!
           WRITE(mcLUN,*) " " 
           WRITE(mcLUN,*) " ERROR IN ",procedurename,"  :" 
           WRITE(mcLUN,*) "       PRODUCED NEGATIVE WEIGHT !!!...k0Min IS TOO SMALL !!!"
           WRITE(mcLUN,*) ""
           STOP 

        ELSEIF (type.EQ.5.AND.error) THEN           ! ElectroWeak Correction ON|OFF !!!
           IF (EWC .EQ. 1d0) THEN          
              WRITE(mcLUN,*) ""
              WRITE(mcLUN,'(3X,A40)') "########################################"
              WRITE(mcLUN,'(3X,A40)') "#                                      #"
              WRITE(mcLUN,'(3X,A40)') "# MESSAGE FROM MC_Init :               #"
              WRITE(mcLUN,'(3X,A40)') "#         EW CORRECTIONS -- SWITCH ON  #"
              WRITE(mcLUN,'(3X,A40)') "#                                      #"
              WRITE(mcLUN,'(3X,A40)') "########################################"
              WRITE(mcLUN,*) ""
           ELSEIF (EWC.EQ.0d0) THEN
              WRITE(mcLUN,*) ""
              WRITE(mcLUN,'(3X,A40)') "########################################"
              WRITE(mcLUN,'(3X,A40)') "#                                      #"
              WRITE(mcLUN,'(3X,A40)') "# MESSAGE FROM MC_Init :               #"
              WRITE(mcLUN,'(3X,A40)') "#         EW CORRECTIONS -- SWITCH OFF #"
              WRITE(mcLUN,'(3X,A40)') "#                                      #"
              WRITE(mcLUN,'(3X,A40)') "########################################"
              WRITE(mcLUN,*) ""
           ELSE
              WRITE(mcLUN,*) ""
              WRITE(mcLUN,'(3X,A38)') "######################################"
              WRITE(mcLUN,'(3X,A38)') "#                                    #"
              WRITE(mcLUN,'(3X,A38)') "#   ERROR IN MC_Init :               #"
              WRITE(mcLUN,'(3X,A38)') "#         EWC FLAG MUST BE :         #"  
              WRITE(mcLUN,'(3X,A38)') "#          1 -- EW correction ON     #" 
              WRITE(mcLUN,'(3X,A38)') "#          0 -- EW correction OFF    #"
              WRITE(mcLUN,'(3X,A38)') "#                                    #" 
              WRITE(mcLUN,'(3X,A38)') "######################################" 
              WRITE(mcLUN,*) ""
              STOP
           ENDIF
        ELSEIF (type.EQ.6.AND.error) THEN           ! Photon Energy Range Checks !!!
                                 
           IF ( k0Max.GT.k0MaxKin ) THEN
              PRINT*, ""
              PRINT*, "ERROR IN MakePhaseSpace_3body : "
              PRINT*, "      INCORRECT VALUE FOR k0Max "
              PRINT*, "             k0Max       = ",k0Max
              PRINT*, "             k0Max MUST <= ",k0MaxKin
              PRINT*, ""
              STOP         
           ELSEIF ( omega.GT.k0MaxKin ) THEN
              PRINT*, ""
              PRINT*, "ERROR IN MakePhaseSpace_3body : "
              PRINT*, "      INCORRECT VALUE FOR omega "
              PRINT*, "             omega    = ",omega
              PRINT*, "          omega MUST <= ",k0MaxKin
              PRINT*, ""
              STOP
          ELSEIF ( k0Max.LT.omega ) THEN
              PRINT*, ""
              PRINT*, "ERROR IN MakePhaseSpace_3body : "
              PRINT*, "      INCORRECT VALUE FOR omega  "
              PRINT*, "           omega    = ",omega
              PRINT*, "        omega MUST <= ",k0Max
              PRINT*, ""
              STOP
          ENDIF 

        ENDIF 
      RETURN
      END
