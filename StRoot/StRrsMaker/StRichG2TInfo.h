/****************************************************************
 * $Id: StRichG2TInfo.h,v 1.1 2000/04/05 15:55:13 lasiuk Exp $
 *
 * Description:
 *   G2T which is drawn in the pad monitor
 *
 ****************************************************************
 *
 * $Log: StRichG2TInfo.h,v $
 * Revision 1.1  2000/04/05 15:55:13  lasiuk
 * Initial Revision
 *
 ****************************************************************/
#ifndef ST_RICH_G2T_INFO
#define ST_RICH_G2T_INFO

struct StRichG2TInfo {
public:
    StRichG2TInfo();
    StRichG2TInfo(double x, double y, int trackp, char* type="u");
    ~StRichG2TInfo();

public:
    double mX;
    double mY;
    int    mTrackp;
    char*  mType;
};
#endif
