/***************************************************************
 * $Id: StRichPadMonitorText.cxx,v 1.1 2000/02/29 18:18:59 lasiuk Exp $
 *
 * Description:
 *  Text window filled by clickable actions onthe padmonitor
 *  Runs only in ROOT
 *
 ***************************************************************
 *
 * $Log: StRichPadMonitorText.cxx,v $
 * Revision 1.1  2000/02/29 18:18:59  lasiuk
 * Initial Revision
 *
 ***************************************************************/
#ifdef __ROOT__

#include "StRichPadMonitorText.h"

StRichPadMonitorText::StRichPadMonitorText()
{
    // Specify the corners
    myLabel = new TPaveLabel(-65,43,-1.5,48,"");  
    myLabel->SetTextAlign(12);
    myLabel->SetTextSize(1.); // .05
    myLabel->SetTextFont(4);  // Arial
    myLabel->Draw();
}

StRichPadMonitorText::~StRichPadMonitorText()
{
    delete myLabel;
}

void StRichPadMonitorText::drawText(StRichDrawableTPad* tpad)
{
    char text[100];
    sprintf(text,"pad= %d row= %d adc = %d\0",tpad->pad(),tpad->row(),tpad->adc());
    myLabel->SetLabel(text);
    myLabel->Draw();
}

void StRichPadMonitorText::removeText()
{
    char text[100];
    sprintf(text," ");
    myLabel->SetLabel(text);
    myLabel->Draw();
}

#endif /* ROOT */
