/* ctf_raw.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
#ifndef CTF_RAW_H
#define CTF_RAW_H
#define CTF_RAW_SPEC \
"struct ctf_raw { \
	long i_eta; \
	long i_phi; \
	long adc; \
	long tdc; \
};"
typedef struct ctf_raw_st {
	long i_eta; /* eta index */
	long i_phi; /* Phi index */
	long adc; /* ADC from scintilator */
	long tdc; /* TDC from scintillator */
} CTF_RAW_ST;
#endif /* CTF_RAW_H */
