/* fgtGain.h */
/* This file was made by the idl compiler "stic". Do not edit.
** This was generated for version '(unspecified)'
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
    fgtGain.idl

  Table: fgtGain

       description: :  Table which contains gain correction information:  Table which contains gain correction information


 */
#ifndef FGTGAIN_H
#define FGTGAIN_H
#define FGTGAIN_SPEC \
"struct fgtGain { \
	double Gain[51200]; \
	octet Status[51200]; \
};"
typedef struct fgtGain_st {
	double Gain[51200]; /*   Gain Variation  */
	unsigned char Status[51200]; /*   status of the channel (0=problem, 1=ok)  */
} FGTGAIN_ST;
#endif /* FGTGAIN_H */
