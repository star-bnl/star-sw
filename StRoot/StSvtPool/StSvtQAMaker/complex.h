typedef struct
{
  double re,
    im;
}
COMPLEX;
#define		c_re(c)		((c).re)
#define		c_im(c)		((c).im)

/*
 * C_add_mul adds product of c1 and c2 to c.
 */
#define	c_add_mul(c, c1, c2)	{ COMPLEX C1, C2; C1 = (c1); C2 = (c2); \
				  c_re (c) += C1.re * C2.re - C1.im * C2.im; \
				  c_im (c) += C1.re * C2.im + C1.im * C2.re; }

/*
 * C_conj substitutes c by its complex conjugate.
 */
#define c_conj(c)		{ c_im (c) = -c_im (c); }

/*
 * C_realdiv divides complex c by real.
 */
#define	c_realdiv(c, real)	{ c_re (c) /= (real); c_im (c) /= (real); }

void Fourier(COMPLEX * in, unsigned int n, COMPLEX * out);
int W_init(unsigned int n);
void realfft(double *in, unsigned int n, double *out);
void realrft(double *in, unsigned int n, double *out);
int fft(COMPLEX * in, unsigned int n, COMPLEX * out);
int rft(COMPLEX * in, unsigned int n, COMPLEX * out);
