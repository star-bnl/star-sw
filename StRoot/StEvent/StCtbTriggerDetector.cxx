/***************************************************************************
 *
 * $Id: StCtbTriggerDetector.cxx,v 2.8 2004/02/11 01:42:09 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbTriggerDetector.cxx,v $
 * Revision 2.8  2004/02/11 01:42:09  ullrich
 * Added new constructor to load data from StTriggerData.
 *
 * Revision 2.7  2002/11/19 22:09:08  ullrich
 * Corrected bug: loop in mips() was off by one.
 *
 * Revision 2.6  2002/11/19 20:21:00  ullrich
 * Added method to sum all mips.
 *
 * Revision 2.5  2001/04/05 04:00:47  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2000/05/09 10:22:17  ullrich
 * Updated to cope with modified dst_TrgDet.idl
 *
 * Revision 2.3  1999/12/20 12:54:45  ullrich
 * Adapted changed in trigger table dst_TrgDet
 *
 * Revision 2.2  1999/10/28 22:24:58  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:28  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StCtbTriggerDetector.h"
#include "tables/St_dst_TrgDet_Table.h"
#include "StTriggerData.h"

static const char rcsid[] = "$Id: StCtbTriggerDetector.cxx,v 2.8 2004/02/11 01:42:09 ullrich Exp $";

ClassImp(StCtbTriggerDetector)

StCtbTriggerDetector::StCtbTriggerDetector()
{
    int i, j, k;
    for(i=0; i<mMaxTrays; i++)
        for(j=0; j<mMaxSlats; j++)
            for(k=0; k<mMaxEventSamples; k++) {
                mMips[i][j][k] = 0;
                mTime[i][j][k] = 0;
            }

    for(i=0; i<mMaxAux; i++)
        for(j=0; j<mMaxEventSamples; j++)
            mAux[i][j] = 0;
    
    mNumberOfPreSamples = 0;
    mNumberOfPostSamples = 0;
}

StCtbTriggerDetector::StCtbTriggerDetector(const dst_TrgDet_st& t)
{
    int i, j, k;
    for(i=0; i<mMaxTrays; i++)
        for(j=0; j<mMaxSlats; j++)
            for(k=0; k<mMaxEventSamples; k++) {
                mMips[i][j][k] = t.nCtb[i][j][k];
                mTime[i][j][k] = t.timeCtb[i][j][k];
            }

    for(i=0; i<mMaxAux; i++)
        for(j=0; j<mMaxEventSamples; j++)
            mAux[i][j] = t.ctbaux[i][j];
    
    mNumberOfPreSamples = t.npre;
    mNumberOfPostSamples = t.npost;
}

StCtbTriggerDetector::StCtbTriggerDetector(const StTriggerData& t)
{
    //
    //  This is a temporary fix only. In future this
    //  class will become obsolete and users should
    //  get this info from StTriggerData.
    //  This info is only a subset of what is available
    //  in StTriggerData.
    //  This stuff is extremely ugly, but for now the
    //  quickest way to get the job done. StTriggerData
    //  has a flat map while the one here is 3dim. Hence
    //  we have to remap. The CTB map for >= 2001 from
    //  Herb's St_trg_Maker.
    //  tu 2/10/2004
    //
    int evtmap[mMaxEventSamples] = {0, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5};
    int ctbmap[mMaxTrays][mMaxSlats];

    // Herb's map
    ctbmap[1-1][1-1]=109;
    ctbmap[2-1][1-1]=108;
    ctbmap[3-1][1-1]=107;
    ctbmap[4-1][1-1]=106;
    ctbmap[5-1][1-1]=105;
    ctbmap[6-1][1-1]=7;
    ctbmap[7-1][1-1]=6;
    ctbmap[8-1][1-1]=5;
    ctbmap[9-1][1-1]=4;
    ctbmap[10-1][1-1]=3;
    ctbmap[11-1][1-1]=2;
    ctbmap[12-1][1-1]=1;
    ctbmap[13-1][1-1]=0;
    ctbmap[14-1][1-1]=15;
    ctbmap[15-1][1-1]=14;
    ctbmap[16-1][1-1]=13;
    ctbmap[17-1][1-1]=12;
    ctbmap[18-1][1-1]=11;
    ctbmap[19-1][1-1]=10;
    ctbmap[20-1][1-1]=9;
    ctbmap[21-1][1-1]=39;
    ctbmap[22-1][1-1]=38;
    ctbmap[23-1][1-1]=37;
    ctbmap[24-1][1-1]=36;
    ctbmap[25-1][1-1]=35;
    ctbmap[26-1][1-1]=34;
    ctbmap[27-1][1-1]=33;
    ctbmap[28-1][1-1]=32;
    ctbmap[29-1][1-1]=47;
    ctbmap[30-1][1-1]=46;
    ctbmap[31-1][1-1]=45;
    ctbmap[32-1][1-1]=44;
    ctbmap[33-1][1-1]=43;
    ctbmap[34-1][1-1]=42;
    ctbmap[35-1][1-1]=41;
    ctbmap[36-1][1-1]=71;
    ctbmap[37-1][1-1]=70;
    ctbmap[38-1][1-1]=69;
    ctbmap[39-1][1-1]=68;
    ctbmap[40-1][1-1]=67;
    ctbmap[41-1][1-1]=66;
    ctbmap[42-1][1-1]=65;
    ctbmap[43-1][1-1]=64;
    ctbmap[44-1][1-1]=79;
    ctbmap[45-1][1-1]=78;
    ctbmap[46-1][1-1]=77;
    ctbmap[47-1][1-1]=76;
    ctbmap[48-1][1-1]=75;
    ctbmap[49-1][1-1]=74;
    ctbmap[50-1][1-1]=73;
    ctbmap[51-1][1-1]=103;
    ctbmap[52-1][1-1]=102;
    ctbmap[53-1][1-1]=101;
    ctbmap[54-1][1-1]=100;
    ctbmap[55-1][1-1]=99;
    ctbmap[56-1][1-1]=98;
    ctbmap[57-1][1-1]=97;
    ctbmap[58-1][1-1]=96;
    ctbmap[59-1][1-1]=111;
    ctbmap[60-1][1-1]=110;
    ctbmap[1-1][2-1]=125;
    ctbmap[2-1][2-1]=124;
    ctbmap[3-1][2-1]=123;
    ctbmap[4-1][2-1]=122;
    ctbmap[5-1][2-1]=121;
    ctbmap[6-1][2-1]=23;
    ctbmap[7-1][2-1]=22;
    ctbmap[8-1][2-1]=21;
    ctbmap[9-1][2-1]=20;
    ctbmap[10-1][2-1]=19;
    ctbmap[11-1][2-1]=18;
    ctbmap[12-1][2-1]=17;
    ctbmap[13-1][2-1]=16;
    ctbmap[14-1][2-1]=31;
    ctbmap[15-1][2-1]=30;
    ctbmap[16-1][2-1]=29;
    ctbmap[17-1][2-1]=28;
    ctbmap[18-1][2-1]=27;
    ctbmap[19-1][2-1]=26;
    ctbmap[20-1][2-1]=25;
    ctbmap[21-1][2-1]=55;
    ctbmap[22-1][2-1]=54;
    ctbmap[23-1][2-1]=53;
    ctbmap[24-1][2-1]=52;
    ctbmap[25-1][2-1]=51;
    ctbmap[26-1][2-1]=50;
    ctbmap[27-1][2-1]=49;
    ctbmap[28-1][2-1]=48;
    ctbmap[29-1][2-1]=63;
    ctbmap[30-1][2-1]=62;
    ctbmap[31-1][2-1]=61;
    ctbmap[32-1][2-1]=60;
    ctbmap[33-1][2-1]=59;
    ctbmap[34-1][2-1]=58;
    ctbmap[35-1][2-1]=57;
    ctbmap[36-1][2-1]=87;
    ctbmap[37-1][2-1]=86;
    ctbmap[38-1][2-1]=85;
    ctbmap[39-1][2-1]=84;
    ctbmap[40-1][2-1]=83;
    ctbmap[41-1][2-1]=82;
    ctbmap[42-1][2-1]=81;
    ctbmap[43-1][2-1]=80;
    ctbmap[44-1][2-1]=95;
    ctbmap[45-1][2-1]=94;
    ctbmap[46-1][2-1]=93;
    ctbmap[47-1][2-1]=92;
    ctbmap[48-1][2-1]=91;
    ctbmap[49-1][2-1]=90;
    ctbmap[50-1][2-1]=89;
    ctbmap[51-1][2-1]=119;
    ctbmap[52-1][2-1]=118;
    ctbmap[53-1][2-1]=117;
    ctbmap[54-1][2-1]=116;
    ctbmap[55-1][2-1]=115;
    ctbmap[56-1][2-1]=114;
    ctbmap[57-1][2-1]=113;
    ctbmap[58-1][2-1]=112;
    ctbmap[59-1][2-1]=127;
    ctbmap[60-1][2-1]=126;
    ctbmap[61-1][1-1]=141;
    ctbmap[62-1][1-1]=140;
    ctbmap[63-1][1-1]=139;
    ctbmap[64-1][1-1]=138;
    ctbmap[65-1][1-1]=137;
    ctbmap[66-1][1-1]=167;
    ctbmap[67-1][1-1]=166;
    ctbmap[68-1][1-1]=165;
    ctbmap[69-1][1-1]=164;
    ctbmap[70-1][1-1]=163;
    ctbmap[71-1][1-1]=162;
    ctbmap[72-1][1-1]=161;
    ctbmap[73-1][1-1]=160;
    ctbmap[74-1][1-1]=175;
    ctbmap[75-1][1-1]=174;
    ctbmap[76-1][1-1]=173;
    ctbmap[77-1][1-1]=172;
    ctbmap[78-1][1-1]=171;
    ctbmap[79-1][1-1]=170;
    ctbmap[80-1][1-1]=169;
    ctbmap[81-1][1-1]=199;
    ctbmap[82-1][1-1]=198;
    ctbmap[83-1][1-1]=197;
    ctbmap[84-1][1-1]=196;
    ctbmap[85-1][1-1]=195;
    ctbmap[86-1][1-1]=194;
    ctbmap[87-1][1-1]=193;
    ctbmap[88-1][1-1]=192;
    ctbmap[89-1][1-1]=207;
    ctbmap[90-1][1-1]=206;
    ctbmap[91-1][1-1]=205;
    ctbmap[92-1][1-1]=204;
    ctbmap[93-1][1-1]=203;
    ctbmap[94-1][1-1]=202;
    ctbmap[95-1][1-1]=201;
    ctbmap[96-1][1-1]=231;
    ctbmap[97-1][1-1]=230;
    ctbmap[98-1][1-1]=229;
    ctbmap[99-1][1-1]=228;
    ctbmap[100-1][1-1]=227;
    ctbmap[101-1][1-1]=226;
    ctbmap[102-1][1-1]=225;
    ctbmap[103-1][1-1]=224;
    ctbmap[104-1][1-1]=239;
    ctbmap[105-1][1-1]=238;
    ctbmap[106-1][1-1]=237;
    ctbmap[107-1][1-1]=236;
    ctbmap[108-1][1-1]=235;
    ctbmap[109-1][1-1]=234;
    ctbmap[110-1][1-1]=233;
    ctbmap[111-1][1-1]=135;
    ctbmap[112-1][1-1]=134;
    ctbmap[113-1][1-1]=133;
    ctbmap[114-1][1-1]=132;
    ctbmap[115-1][1-1]=131;
    ctbmap[116-1][1-1]=130;
    ctbmap[117-1][1-1]=129;
    ctbmap[118-1][1-1]=128;
    ctbmap[119-1][1-1]=143;
    ctbmap[120-1][1-1]=142;
    ctbmap[61-1][2-1]=157;
    ctbmap[62-1][2-1]=156;
    ctbmap[63-1][2-1]=155;
    ctbmap[64-1][2-1]=154;
    ctbmap[65-1][2-1]=153;
    ctbmap[66-1][2-1]=183;
    ctbmap[67-1][2-1]=182;
    ctbmap[68-1][2-1]=181;
    ctbmap[69-1][2-1]=180;
    ctbmap[70-1][2-1]=179;
    ctbmap[71-1][2-1]=178;
    ctbmap[72-1][2-1]=177;
    ctbmap[73-1][2-1]=176;
    ctbmap[74-1][2-1]=191;
    ctbmap[75-1][2-1]=190;
    ctbmap[76-1][2-1]=189;
    ctbmap[77-1][2-1]=188;
    ctbmap[78-1][2-1]=187;
    ctbmap[79-1][2-1]=186;
    ctbmap[80-1][2-1]=185;
    ctbmap[81-1][2-1]=215;
    ctbmap[82-1][2-1]=214;
    ctbmap[83-1][2-1]=213;
    ctbmap[84-1][2-1]=212;
    ctbmap[85-1][2-1]=211;
    ctbmap[86-1][2-1]=210;
    ctbmap[87-1][2-1]=209;
    ctbmap[88-1][2-1]=208;
    ctbmap[89-1][2-1]=223;
    ctbmap[90-1][2-1]=222;
    ctbmap[91-1][2-1]=221;
    ctbmap[92-1][2-1]=220;
    ctbmap[93-1][2-1]=219;
    ctbmap[94-1][2-1]=218;
    ctbmap[95-1][2-1]=217;
    ctbmap[96-1][2-1]=247;
    ctbmap[97-1][2-1]=246;
    ctbmap[98-1][2-1]=245;
    ctbmap[99-1][2-1]=244;
    ctbmap[100-1][2-1]=243;
    ctbmap[101-1][2-1]=242;
    ctbmap[102-1][2-1]=241;
    ctbmap[103-1][2-1]=240;
    ctbmap[104-1][2-1]=255;
    ctbmap[105-1][2-1]=254;
    ctbmap[106-1][2-1]=253;
    ctbmap[107-1][2-1]=252;
    ctbmap[108-1][2-1]=251;
    ctbmap[109-1][2-1]=250;
    ctbmap[110-1][2-1]=249;
    ctbmap[111-1][2-1]=151;
    ctbmap[112-1][2-1]=150;
    ctbmap[113-1][2-1]=149;
    ctbmap[114-1][2-1]=148;
    ctbmap[115-1][2-1]=147;
    ctbmap[116-1][2-1]=146;
    ctbmap[117-1][2-1]=145;
    ctbmap[118-1][2-1]=144;
    ctbmap[119-1][2-1]=159;
    ctbmap[120-1][2-1]=158;
    
    int i, j, k, pmt, evt;
    
    for(i=0; i<mMaxTrays; i++)
        for(j=0; j<mMaxSlats; j++)
            for(k=0; k<mMaxEventSamples; k++) {
		pmt = ctbmap[i][j];
		evt = evtmap[k];
		mMips[i][j][k] = t.ctb(pmt, evt);
                mTime[i][j][k] = 0;              // N/A
            }
    
    for(i=0; i<mMaxAux; i++)
        for(j=0; j<mMaxEventSamples; j++)
            mAux[i][j] = 0;                      // N/A
    
    mNumberOfPreSamples = 5;
    mNumberOfPostSamples = 5;
}

StCtbTriggerDetector::~StCtbTriggerDetector() {/* noop */}

unsigned int
StCtbTriggerDetector::numberOfTrays() const {return mMaxTrays;}

unsigned int
StCtbTriggerDetector::numberOfSlats() const {return mMaxSlats;}

unsigned int
StCtbTriggerDetector::numberOfAuxWords() const {return mMaxAux;}

unsigned int
StCtbTriggerDetector::numberOfPreSamples() const {return mNumberOfPreSamples;}

unsigned int
StCtbTriggerDetector::numberOfPostSamples() const {return mNumberOfPostSamples;}

float
StCtbTriggerDetector::mips(unsigned int i, unsigned int j, unsigned int k) const
{
    return mMips[i][j][k];
}

double
StCtbTriggerDetector::mips(unsigned int evt) const
{
    double sum = 0;
    for (unsigned int i=0; i<mMaxTrays; i++) 
	for (unsigned int j=0; j<mMaxSlats; j++)
	    sum += mMips[i][j][evt];
    return sum;
}

char
StCtbTriggerDetector::time(unsigned int i, unsigned int j, unsigned int k) const
{
    return mTime[i][j][k];
}
    
float
StCtbTriggerDetector::aux(unsigned int i, unsigned int j) const
{
    return mAux[i][j];
}

void
StCtbTriggerDetector::setMips(unsigned int i, unsigned int j, unsigned int k, float val)
{
    mMips[i][j][k] = val;
}

void
StCtbTriggerDetector::setTime(unsigned int i, unsigned int j, unsigned int k, char val)
{
    mTime[i][j][k] = val;
}

void
StCtbTriggerDetector::setAux(unsigned int i, unsigned int j, float val)
{
    mAux[i][j] = val;
}

void
StCtbTriggerDetector::setNumberOfPreSamples(unsigned int val)
{
    mNumberOfPreSamples = val;
}

void
StCtbTriggerDetector::setNumberOfPostSamples(unsigned int val)
{
    mNumberOfPostSamples = val;
}

