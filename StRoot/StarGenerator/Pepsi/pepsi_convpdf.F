      SUBROUTINE PEPSI_CONVPDF(input,ioutput,lok)
******************************************************************************
* Author: Bjoern Seitz (bseitz@hermes.desy.de)
*
* Purpose: Workaround to make PEPSI compatible to PDF lib numbers
*          This is a cheap trick and only works for PDFs intrisically
*          known to PEPSI
*
* Input: PDFLIB number
* Output: PEPSI compliant PDF stored in LST(15);LST(16) is set to 1
****************************************************************************** 
#include "lepto2.inc"
#include "pepadm.inc"

      logical lok               ! true if conversion was successful
      integer input             ! PDFlib number 
      integer ioutput

      lok=.false.               ! per default, no change to selected partonset

      ioutput=input             ! assume that human did something reasonable
C these ones are known to PEPSI
      if (input.eq.5010) then   ! GRSV NLO standard
         LST(15)=116         
         ioutput=116
         lok=.true.
      endif

      if (input.eq.5011) then   ! GRSV NLO valence
         LST(15)=117         
         ioutput=117
         lok=.true.
      endif

      if (input.eq.5008) then   ! GRSV LO standard
         LST(15)=118         
         ioutput=118
         lok=.true.
      endif

      if (input.eq.5009) then   ! GRSV LO valence
         LST(15)=119 
         ioutput=119        
         lok=.true.
      endif

      if (input.eq.4041) then   ! CTEQ4LQ
         LST(15)=133
         ioutput=133            
         lok=.true.
      endif
      if (input.eq.3043) then   ! MRS low Q^2
         LST(15)=137
         ioutput=137
         lok=.true.
      endif

! If chosen PDFLIB set is being replaced by PEPSI-internal version,
! inform the user and change LST(16) to 1.

	if (lok) then
	  lst(16) = 1
	  write(6,'(a,i4,a,i3)') 
     +		'PEPSI_CONVPDF: replacing PDFLIB partonset ',
     +		input, ' with PEPSI version ', ioutput
	endif

      return
      end
