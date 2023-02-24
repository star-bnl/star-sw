#ifndef _SUPPORT_H_
#define _SUPPORT_H_

#include <math.h>

// real time in useconds
inline unsigned long t_mark()
{
        timespec ts ;

        clock_gettime(CLOCK_MONOTONIC,&ts) ;

        return (unsigned long)ts.tv_sec*1000000+(unsigned long)ts.tv_nsec/1000 ;
}

// returns microseconds; good enough
inline unsigned int t_delta(unsigned long mark)
{
        return (unsigned int)((t_mark() - mark)) ;
}



class t_ave
{
public:
	t_ave(const char *yada) {
		text = yada ;
		clr() ;
		mark() ;
	}

	~t_ave() {;} ;

	void mark() {
		my_mark = t_mark() ;
	}

	void ave() {
		unsigned int delta = t_delta(my_mark) ;

		mean += delta ;
		rms += delta*delta ;
		cou++ ;

		my_mark = t_mark() ;

	}

	void clr() {
		mean = rms = 0.0 ;
		cou = 0 ;
	}

	void calc() {
		double d_mean = mean/cou ;
		double d_rms = rms/cou ;

		d_rms = sqrt(d_rms-d_mean*d_mean) ;

		LOG(INFO,"t_ave[%s] = %.1f us +- %.1f [%u]",text,d_mean,d_rms,cou) ;

	}


	const char *text ;
	double mean ;
	double rms ;
	unsigned int cou ;

	unsigned long my_mark ;
} ;


#endif
