#include "StTFourierTransform.h"
#include "complex.h"
#include "w.h"
#include "malloc.h"
//#include "free.h"
#include "TMath.h"
COMPLEX *W_factors = 0;		/* array of W-factors */
unsigned Nfactors = 0;		/* number of entries in W-factors */
static unsigned radix(unsigned n);
static void split(COMPLEX * in, unsigned r, unsigned m, COMPLEX * out);
static void join(COMPLEX * in, unsigned m, unsigned n, COMPLEX * out);

ClassImp(StTFourierTransform)

void StTFourierTransform::Fft(TH1 * in, TH1 * amplitude, TH1 * phase,
  UInt_t start, UInt_t end)
{
  if (start == 0 && end == 0)
  {
    start = 1;
    end = in->GetNbinsX();
  }

  UInt_t nbins = end - start + 1;

  Double_t *inArray = new Double_t[nbins];

  // we assume that the histogram has equal-width bins
  Double_t width = in->GetBinWidth(start);

  UInt_t i;

  for (i = 0; i < nbins; i++)
  {
    inArray[i] = in->GetBinContent(i + start);
  }

  Double_t *outArray = new Double_t[nbins];

  realfft(inArray, nbins, outArray);

  delete[]inArray;

  Double_t outWidth = 1 / (in->GetBinCenter(nbins) - in->GetBinCenter(1) + width);

  if (amplitude)
  {
    amplitude->SetBins(nbins / 2, -outWidth / 2, ((Double_t) (nbins - 1)) /
      2 * outWidth);
    amplitude->SetBinContent(1, outArray[0]);

    for (i = 1; i < nbins / 2; i++)
    {
      amplitude->SetBinContent(i + 1, TMath::Sqrt(TMath::Power(outArray[2 * i - 1], 2) +
	  TMath::Power(outArray[2 * i], 2)));
    }
  }

  if (phase)
  {
    phase->SetBins(nbins / 2, -outWidth / 2, ((Double_t) (nbins - 1) / 2) * outWidth);
    phase->SetBinContent(1, 0);

    for (i = 1; i < nbins / 2; i++)
    {
      phase->SetBinContent(i + 1, TMath::ATan2(outArray[2 * i], outArray[2 * i - 1]));
    }
  }

  delete[]outArray;

  return;
}

//***********************************************************************************************
void realfft(double *in, unsigned n, double *out)
{
  COMPLEX *c_in,
   *c_out;
  unsigned i;

  if (n == 0 ||
    (c_in = (COMPLEX *) malloc(n * sizeof(COMPLEX))) == 0 ||
    (c_out = (COMPLEX *) malloc(n * sizeof(COMPLEX))) == 0)
    return;

  for (i = 0; i < n; i++)
  {
    c_re(c_in[i]) = in[i];
    c_im(c_in[i]) = 0;
  }

  fft(c_in, n, c_out);

  out[0] = c_re(c_out[0]);	/* TMath::Cos van dc */
  for (i = 1; i < (n + 1) / 2; i++)
  {				/* TMath::Cos/TMath::Sin i-de harmonische */
    out[2 * i - 1] = c_re(c_out[i]) * 2;
    out[2 * i] = c_im(c_out[i]) * -2;
  }
  if (n % 2 == 0)		/* TMath::Cos van Nyquist */
    out[n - 1] = c_re(c_out[n / 2]);

  free((char *) c_in);
  free((char *) c_out);
}

/* Forward Fast Fourier Transform on the n samples of complex array in.
 * The result is placed in out.  The number of samples, n, is arbitrary.
 * The W-factors are calculated in advance.
 */
int fft(COMPLEX * in, unsigned n, COMPLEX * out)
{
  unsigned i;

  for (i = 0; i < n; i++)
    c_conj(in[i]);

  if (W_init(n) == -1)
    return -1;

  Fourier(in, n, out);

  for (i = 0; i < n; i++)
  {
    c_conj(out[i]);
    c_realdiv(out[i], n);
  }

  return 0;
}

/*
 * Recursive (reverse) complex fast Fourier transform on the n
 * complex samples of array in, with the Cooley-Tukey method.
 * The result is placed in out.  The number of samples, n, is arbitrary.
 * The algorithm costs O (n * (r1 + .. + rk)), where k is the number
 * of factors in the prime-decomposition of n (also the maximum
 * depth of the recursion), and ri is the i-th primefactor.
 */
void Fourier(COMPLEX * in, unsigned n, COMPLEX * out)
{
  unsigned r;

  if ((r = radix(n)) < n)
    split(in, r, n / r, out);
  join(in, n / r, n, out);
}

/*
 * Give smallest possible radix for n samples.
 * Determines (in a rude way) the smallest primefactor of n.
 */
static unsigned radix(unsigned n)
{
  unsigned r;

  if (n < 2)
    return 1;

  for (r = 2; r < n; r++)
    if (n % r == 0)
      break;
  return r;
}

/*
 * Split array in of r * m samples in r parts of each m samples,
 * such that in [i] goes to out [(i % r) * m + (i / r)].
 * Then call for each part of out Fourier, so the r recursively
 * transformed parts will go back to in.
 */
static void split(COMPLEX * in, unsigned r, unsigned m, COMPLEX * out)
{
  register unsigned k,
    s,
    i,
    j;

  for (k = 0, j = 0; k < r; k++)
    for (s = 0, i = k; s < m; s++, i += r, j++)
      out[j] = in[i];

  for (k = 0; k < r; k++, out += m, in += m)
    Fourier(out, m, in);
}

/* Sum the n / m parts of each m samples of in to n samples in out.
 * 		   r - 1
 * Out [j] becomes  sum  in [j % m] * W (j * k).  Here in is the k-th
 * 		   k = 0   k	       n		 k
 * part of in (indices k * m ... (k + 1) * m - 1), and r is the radix.
 * For k = 0, a complex multiplication with W (0) is avoided.
 */
static void join(COMPLEX * in, unsigned m, unsigned n, COMPLEX * out)
{
  register unsigned i,
    j,
    jk,
    s;

  for (s = 0; s < m; s++)
    for (j = s; j < n; j += m)
    {
      out[j] = in[s];
      for (i = s + m, jk = j; i < n; i += m, jk += j)
	c_add_mul(out[j], in[i], W(n, jk));
    }
}

/*
 * W_init puts Wn ^ k (= e ^ (2pi * i * k / n)) in W_factors [k], 0 <= k < n.
 * If n is equal to Nfactors then nothing is done, so the same W_factors
 * array can used for several transforms of the same number of samples.
 * Notice the explicit calculation of sines and cosines, an iterative approach
 * introduces substantial errors.
 */
int W_init(unsigned n)
{
#	define pi	3.1415926535897932384626434
  unsigned k;

  if (n == Nfactors)
    return 0;
  if (Nfactors != 0 && W_factors != 0)
    free((char *) W_factors);
  if ((Nfactors = n) == 0)
    return 0;
  if ((W_factors = (COMPLEX *) malloc(n * sizeof(COMPLEX))) == 0)
    return -1;

  for (k = 0; k < n; k++)
  {
    c_re(W_factors[k]) = TMath::Cos(2 * pi * k / n);
    c_im(W_factors[k]) = TMath::Sin(2 * pi * k / n);
  }

  return 0;
}
