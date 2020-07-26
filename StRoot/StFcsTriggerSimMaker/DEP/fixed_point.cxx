#include <stdio.h>
#include <sys/types.h>

int main()
{
	u_short adc = 123 ;

	double pt_corr = 4.7 ;
	double gain_corr = 1.4 ;

	u_int int_corr = (u_short)(pt_corr*gain_corr*64.0+0.5) ;
	int_corr &= 0x3FF ;	// 10 bits only!!!

	u_int adc1 = adc * int_corr ;
	u_short adc2 = (adc1>>6) ;

	printf("%d -> %d -> %d [%f]\n",adc,adc1,adc2,(double)adc*pt_corr*gain_corr) ;

	return 0 ;
}
