typedef struct fgtPedestal_st {
    double AdcPedestal[51200];     /* ADC pedestal */
    double AdcPedestalRMS[51200];     /* ADC pedestal RMS */
    unsigned char Status[51200];     /* Status (0=problem, 1=ok) */
} FGTPEDESTAL_ST;
