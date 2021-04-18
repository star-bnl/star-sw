/* itpcPadGainT0.h */
/* This file was made by the idl compiler "stic". Do not edit.
** This was generated for version '(unspecified)'
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
    itpcPadGainT0.idl

  Table: itpcPadGainT0

       description:

 */
#ifndef ITPCPADGAINT0_H
#define ITPCPADGAINT0_H
#define ITPCPADGAINT0_SPEC \
"struct itpcPadGainT0 { \
        long run; \
        float Gain[24][40][120]; \
        float T0[24][40][120]; \
};"
typedef struct itpcPadGainT0_st {
        int run; /* pulser run number used */
        float Gain[24][40][120]; /* Gains per pad*/
        float T0[24][40][120]; /* T0 per pad*/
} ITPCPADGAINT0_ST;
#endif /* ITPCPADGAINT0_H */
