/***************************************************************************
 *
 * $Id: StRichUtilities.h,v 1.1 2001/08/27 16:43:43 lasiuk Exp $
 *
 * Author:  bl
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              Utility for packing data
 *              
 ***************************************************************************
 *
 * $Log: StRichUtilities.h,v $
 * Revision 1.1  2001/08/27 16:43:43  lasiuk
 * Initial Revision
 *
 ***************************************************************************/
#ifndef ST_RICH_UTILITIES_hh
#define ST_RICH_UTILITIES_hh


unsigned long packAdc(unsigned long pad, unsigned long row, unsigned long adc) {

    unsigned long codedValue = (adc << 16) | (row << 8) | pad;
    return  codedValue;
}

void unPackAdc(unsigned long coded, unsigned long *pad, unsigned long *row, unsigned long *adc) {

    *pad = ( coded        & 0xff);
    *row = ((coded >> 8)  & 0xff);
    //--> Used to be *adc = ((code >> 16) & 0x3ff);
    *adc = ( (coded>>26) & 0x1 ) ? 1024 : ( (coded>>16) & 0x3ff);
}

#endif
