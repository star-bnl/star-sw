///////////////////////////////////////////////////////////////////////////////
// sca_str.hh
// This class is highly dependent on scale.d which is uses for initialization
///////////////////////////////////////////////////////////////////////////////
#ifndef _sca_str_included_
#define _sca_str_included_

#include <fstream.h>
#include <string.h>
#include <stdlib.h>
#define MAX_RANKS 10
#define WORD_SIZE 40
#define PRIOR_SIZE 300000

//  DEBUG and ERROR levels 
const int SILENT = 0;
const int LOW    = 1;
const int MEDIUM = 2;
const int HIGH   = 3;

#ifndef TRUE 
#define   TRUE   1;
#endif

#ifndef FALSE
#define   FALSE  0;
#endif

#ifndef OK
#define OK     1;
#endif

#ifndef FAIL
#define FAIL   0;
#endif

//Misc 
const int MaxLineLength = 256;
const float BINSIZE = 1.0;      // default bin size and range 
const float EPSILON = 1e-6;     // to prevent division by 0, etc. 

struct sca_data_t {
  int iflag;
  double x;
  double y;
  double z;
  int   evn;                    // placeholder for the future 
  int   run;
}; 

typedef struct sca_info_t {
  float scale_x;                // log10(binsize/datasize) in x 
  float scale_y;                // log10(binsize/datasize) in y 
  float rank;                   // entropy is this rank (q) Renyi entropy 
  float volume;                 // Correlation integral C_q (volume) */
                                // C_q = sum over occup. bins of (n_i/N)^q 
  float entropy;                // S_q = 1/(1-q) log10 [C_q] 
  float dim;                    // negative slope of entropy in scale 
  float info;                   // Difference I_q from reference entropy 
                                // I_q = S_q - S_qref 
  float dim_lower;              // Dimension lowering; diff. from refr. 
                                // \delta d_q = d_q - d_qref 
  int evn;                      // event number 
  int run;                      // run number 
} sca_info_t;

typedef struct sca_info_t sca_info;
typedef struct sca_info_t sca_ref_info;

typedef struct sca_t {
  float rank;                   // the rank of the analysis 
  int num_scales;               // the num. of scales used in calculations 
  sca_info_t *info;             // ptr to the information for this rank 
} sca_t;

typedef struct sca_prior_t {
  float  w;
  int marker;
} sca_prior_t;

typedef struct sca_const_t {
  long    num_dim;
  float   minbinsize_x;
  float   maxbinsize_x;
  float   minbinsize_y;
  float   maxbinsize_y;
  float   step_binsize_x;
  float   step_binsize_y;
  float   auto_size;
  float   xmin;
  float   xmax;
  float   ymin;
  float   ymax;
  float   zmin;
  float   zmax;
  long    num_ranks;
  float   rank[10];
  long    dithmin;
  float   dithscale;
  float   perfectdither;  
}sca_const_t;

#endif
