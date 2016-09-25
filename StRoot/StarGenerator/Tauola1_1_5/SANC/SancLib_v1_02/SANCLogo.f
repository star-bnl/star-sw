
      SUBROUTINE SANCLogo(index)

      INTEGER index
      INCLUDE 'MC_Declare.h'

      IF (index .eq. 1) THEN

         print*, "             "
         print*, "         ","##########################################################"
         print*, "             "
         print*, "             ","SSSSSSSSSS   AAAAA        NNN     NN   CCCCCCCCCC"
         print*, "             ","SS           AA  AA       NN N    NN   CC      CC"
         print*, "             ","SS           AAAAAAAAA    NN  N   NN   CC        "
         print*, "             ","SSSSSSSSSS   AA    AA     NN   N  NN   CC        "
         print*, "             ","        SS   AA     AA    NN    N NN   CC      CC"
         print*, "             ","SSSSSSSSSS   AA      AA   NN     NNN   CCCCCCCCCC"
         print*, "             "
         print*, "             ","             __MONTE CARLO OUTPUT__                   "
         print*, "             "
         print*, "         ","##########################################################"
         print*, "         ","##########################################################"
         print*, "             "

      ENDIF

      RETURN
      END
