/* ctf_raw.inc */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
#ifndef CTF_RAW_INC
#define CTF_RAW_INC
#endif ! CTF_RAW_INC
	STRUCTURE /CTF_RAW_ST/
	INTEGER*4 i_eta /* eta index */
	INTEGER*4 i_phi /* Phi index */
	INTEGER*4 adc /* ADC from scintilator */
	INTEGER*4 tdc /* TDC from scintillator */
	END STRUCTURE ! CTF_RAW_ST !
CCCCCCCCCCCCCCCCCCCCCCCCCCCCC begin: new as of 960429 
      CHARACTER*(*) CTF_RAW_SPEC
      PARAMETER    (CTF_RAW_SPEC=
     + ' struct ctf_raw {'                  //
     + ' long i_eta;'                       //
     + ' long i_phi;'                       //
     + ' long adc;'                         //
     + ' long tdc;'                         //
     + ' };')
CCCCCCCCCCCCCCCCCCCCCCCCCCCCC end: new as of 960429 
