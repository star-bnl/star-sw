///////////////////////////////////////////////////////////////////////////////
// Dataset.hh
/////////////////////////////////////////////////////////////////////////////// 

#ifndef _dataset_included_
#define _dataset_included_

#include "PAM.h"
#include "Histogram.hh"
#include "sca_const.h"
#include "sca_prior.h"
#include "sca_in.h"
#include "sca_out.h"

class Dataset {
public:
  Dataset(  
	  TABLE_HEAD_ST   *sca_const_h,     SCA_CONST_ST     *sca_const,
	  TABLE_HEAD_ST   *sca_prior_h,     SCA_PRIOR_ST     *sca_prior,  
	  TABLE_HEAD_ST   *sca_in_h,        SCA_IN_ST        *sca_in,
	  char *input_data_name,
	  char *output_sca_name );
  ~Dataset();
  int  CalcEntropy();
  int  CalcDimension(void);
  int  fillScaOutTable(TABLE_HEAD_ST   *sca_out_h, SCA_OUT_ST *sca_out);
  int  fillPrior(TABLE_HEAD_ST   *sca_prior_h, SCA_PRIOR_ST *sca_prior);

private:
  int     getConstants(TABLE_HEAD_ST *sca_const_h, SCA_CONST_ST *sca_const);
  int     getPrior(TABLE_HEAD_ST *sca_prior_h, SCA_PRIOR_ST *sca_prior);
  int     getinData(TABLE_HEAD_ST *sca_in_h, SCA_IN_ST  *sca_in);
  void    CalcVolume(int pass, double binsize_x, double binsize_y);
  void    DithVolume(double binsize_x, double binsize_y);
    
  double *vol;                 // SCA   volume
  sca_t        *sca;           // the scaled correlation analysis info
  sca_info_t   *info;          // pointer to all info for all ranks and scales
  sca_data_t  *data;	      // input data to SCA
  sca_data_t  **xdata;	      // Pointers to data sorted by x
  sca_data_t  **ydata;	      // Pointers to data sorted by y
  sca_prior_t  *prior_p;      // pointer to BASIAN reference
  sca_const_t  *constants;    // pointer to SCA constants

  Histogram Hist;	      // Histogram object
  
  int  datanum;	  	      // Number of data points
  int  validnum;	      // Number of data points within simple cut range
  char sca_name[MaxLineLength];
  char sca_info_name[MaxLineLength];


  double xmin, xmax, ymin, ymax; // size of the dataset, independent of points
  void findrange(void);

  int num_ranks;          // number of different ranks analyzed
  int num_scales;         // number of different scales analyzed
  int alloc_info();       // allocate entries in the server.

  double vol_x[MAX_RANKS],	// The phase averaged volumes for when e > L
         vol_y[MAX_RANKS],	// and the boundary is crossing x, y, or both.
         vol_xy[MAX_RANKS];


};  // end Dataset

#endif  // _Dataset_included_
