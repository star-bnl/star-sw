/* Header file for tht */
#ifndef GLOBAL
#define         GLOBAL          extern
#else
#define         INIT            1
#endif

/* general includes */
#include        <stdio.h>
#include        <stdlib.h>
#include        <string.h>
#include        <math.h>
#include        "tas_structs.h"
#include        "tas_user_codes.h"
#include        "tfc_tfcopt_pars.h"
#include        "tfc_tfcopt_st.h"
#include        "tfc_pixel_pars.h"
#include        "tfc_pixel_st.h"
#include        "tss_tppad_pars.h"
#include        "tss_tppad_st.h"
#include        "tss_tppixel_pars.h"
#include        "tss_tppixel_st.h"
#include        "rdio.h"

/* prototypes */
int tfc_main_ (Table_head_st  *h_tfcopt,
               Tfcopt_row_st  *tfcopt,
               Table_head_st  *h_tppad,
               Tppad_row_st   *tppad,
               Table_head_st  *h_tppixel, 
               Tppixel_row_st *tppixel,
               Table_head_st  *h_pixel, 
               Pixel_row_st   *pixel);

#undef GLOBAL
#ifdef INIT
#undef INIT
#endif
