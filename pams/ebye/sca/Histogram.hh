///////////////////////////////////////////////////////////////////////////////
// Histogram.h
// Assume only a 2D histogram for now.
///////////////////////////////////////////////////////////////////////////////

#ifndef _histogram_included_
#define _histogram_included_

#include "sca_const.h"
#include "sca_str.hh"

const int HistMAXPOINTS = 50000;	// Max points in a histogram unless
					// SetMaxPoints is called

class Histogram {
public:
  Histogram(void);
  void SetBinSize(double size);
  void SetBinSize(double xsize, double ysize);
  void SetRange(double xminl, double xmaxl, double yminl, double ymaxl);
  void AddDatum(sca_data_t &datum);
  void AddDatum(double x, double y);
  void AddDatum(sca_data_t **xdata, int numvalid, double x, double y);
  void ClearHisto(void);	// Clear histogram to 0
  void SetMaxPoints(int n);	// Optional: Set number of points to use

  double GetVolume(double rank,
		   int bin_num,
		   int num_dim, 
		   sca_prior_t *prior_p);
  double GetBinSizeX(void);
  double GetBinSizeY(void);

private:
  int N;			// Number of points in histogram
  int NMax;			// Maximum number of points in the histogram
  int *binlist;			// The list of which bin for which point.
  int sorted;			// has the binlist been sorted?

  int nbinsx, nbinsy;		// Number of the bins in x and y
  double binsize_x, binsize_y;	// Size of bins in x and y
  double xmin, xmax, ymin, ymax;	// Histogram ranges

  void SetBinNum(void);		// Set binnum based upon size and range
  void FreeHisto(void);
  void AllocHisto(void);
  void SortBinList(void);
};  // end Histogram

#endif  // _Histogram_included_
