/*------------------------------------------------------------------------------
|
| drawTables.C
| Macro that draws the content of tables saved in the database
|
|
| First argument is the table name
| 
| Second and Third arguments correspond to the timestamp limits for the
| tables in the database
|
| Fourth argument is the maximum entry time in the database. It means that the
| table had to be *saved* before that time. Do not mistake this as the
| timestamp
|
| Fifth argument is the flavor the tables is save in the database
|
| author: Alex Suaide (c) 2006
|
|-----------------------------------------------------------------------------*/

class StEmcDbHandler;
std::vector<std::string> list;
int index = -1;
StEmcDbHandler* dbHandler = NULL;
TString table;

void drawTables(char* t   = "bemcStatus",
    char* TSi = "2001-01-01 00:00:00", 
    char* TSf = "2030-01-01 00:00:00",
    char* MAX = "2030-01-01 00:00:00",
    char* flavor = "ofl")
{
    gROOT->Macro("loadMuDst.C");
    gROOT->LoadMacro("$STAR/StRoot/StEmcUtil/macros/drawSingleTable.C");

    table = t;

    dbHandler=new StEmcDbHandler();
    dbHandler->setTableName(t);
    dbHandler->setMaxEntryTime(MAX);
    dbHandler->setFlavor(flavor);
    list = dbHandler->getTimeStampList(TSi,TSf);

    stats();
    plot(0);
}

void stats()
{
    for(int i =0;i<list.size();i++)
    {
        dbHandler->setTimeStamp(list[i].c_str());
        int nGood = 0;
        int nEmpty = 0;
        int nBad = 0;
        ////////////////////////////////////////////////////////////////
        if(!strcmp("bemcPed",table.Data()) || !strcmp("bprsPed",table.Data()))         
        {
            emcPed = (emcPed_st*) dbHandler->getDbTable()->GetTable();
            for(int j = 0;j<4800;j++) 
                if(emcPed->Status[j] == 1) nGood++; 
            else if(emcPed->Status[j] == 0) nEmpty++; 
            else nBad++; 
        }
        if(!strcmp("bemcCalib",table.Data()) || !strcmp("bprsCalib",table.Data()))     
        {
            emcCalib = (emcCalib_st*) dbHandler->getDbTable()->GetTable();
            for(int j = 0;j<4800;j++)  
                if(emcCalib->Status[j] == 1) nGood++; 
            else if(emcCalib->Status[j] == 0) nEmpty++; 
            else nBad++; 
        } 
        if(!strcmp("bemcStatus",table.Data()) || !strcmp("bprsStatus",table.Data()))   
        {
            emcStatus = (emcStatus_st*) dbHandler->getDbTable()->GetTable();
            for(int j = 0;j<4800;j++)  
                if(emcStatus->Status[j] == 1) nGood++; 
            else if(emcStatus->Status[j] == 0) nEmpty++; 
            else nBad++; 
        } 
        if(!strcmp("bemcGain",table.Data()) || !strcmp("bprsGain",table.Data()))       
        {
            emcGain = (emcGain_st*) dbHandler->getDbTable()->GetTable(); 
            for(int j = 0;j<4800;j++) 
                if(emcGain->Status[j] == 1) nGood++; 
            else if(emcGain->Status[j] == 0) nEmpty++; 
            else nBad++; 
        }
        ////////////////////////////////////////////////////////////////
        if(!strcmp("bsmdePed",table.Data()) || !strcmp("bsmdpPed",table.Data()))       
        {
            smdPed = (smdPed_st*) dbHandler->getDbTable()->GetTable(); 
            for(int j = 0;j<18000;j++) 
                if (smdPed->Status[j] == 1) nGood++; 
            else if(smdPed->Status[j] == 0) nEmpty++; 
            else nBad++; 
        }
        if(!strcmp("bsmdeCalib",table.Data()) || !strcmp("bsmdpCalib",table.Data()))   
        {
            smdCalib = (smdCalib_st*) dbHandler->getDbTable()->GetTable(); 
            for(int j = 0;j<18000;j++) 
                if (smdCalib->Status[j] == 1) nGood++; 
            else if(smdCalib->Status[j] == 0) nEmpty++; 
            else nBad++; 
        }
        if(!strcmp("bsmdeStatus",table.Data()) || !strcmp("bsmdpStatus",table.Data())) 
        {
            smdStatus = (smdStatus_st*) dbHandler->getDbTable()->GetTable();
            for(int j = 0;j<18000;j++) 
                if (smdStatus->Status[j] == 1) nGood++; 
            else if(smdStatus->Status[j] == 0) nEmpty++; 
            else nBad++; 
        } 
        if(!strcmp("bsmdeGain",table.Data()) || !strcmp("bsmdpGain",table.Data()))     
        {
            smdGain = (smdGain_st*) dbHandler->getDbTable()->GetTable();
            for(int j = 0;j<18000;j++) 
                if (smdGain->Status[j] == 1) nGood++; 
            else if(smdGain->Status[j] == 0) nEmpty++; 
            else nBad++; 
        } 
        char line[200];
        sprintf(line,"    %4d   %s   Good channels = %4d  Empty = %5d  BAD = %5d\n",i,list[i].c_str(),nGood,nEmpty,nBad);
        cout <<line;
    }
    cout <<"List size = "<<list.size()<<endl<<endl;
    cout <<"Type:\n";
    cout <<"   next()          for the next plot in the list\n";
    cout <<"   previous()      for the previous plot in the list\n";
    cout <<"   plot(i)         for the i-entry in the list\n";
    cout <<"   stats()         for this statistics\n";
}
void plot(int I)
{
    if(I>=list.size() || I <0) return;
    std::string a = list[I];
    index = I;
    dbHandler->setTimeStamp(a.c_str());
    ////////////////////////////////////////////////////////////////
    if(!strcmp("bemcPed",table.Data()) || !strcmp("bprsPed",table.Data()))         draw((emcPed_st*) dbHandler->getDbTable()->GetTable()); 
    if(!strcmp("bemcCalib",table.Data()) || !strcmp("bprsCalib",table.Data()))     draw((emcCalib_st*) dbHandler->getDbTable()->GetTable()); 
    if(!strcmp("bemcStatus",table.Data()) || !strcmp("bprsStatus",table.Data()))   draw((emcStatus_st*) dbHandler->getDbTable()->GetTable()); 
    if(!strcmp("bemcGain",table.Data()) || !strcmp("bprsGain",table.Data()))       draw((emcGain_st*) dbHandler->getDbTable()->GetTable()); 
    ////////////////////////////////////////////////////////////////
    if(!strcmp("bsmdePed",table.Data()) || !strcmp("bsmdpPed",table.Data()))       draw((smdPed_st*) dbHandler->getDbTable()->GetTable()); 
    if(!strcmp("bsmdeCalib",table.Data()) || !strcmp("bsmdpCalib",table.Data()))   draw((smdCalib_st*) dbHandler->getDbTable()->GetTable()); 
    if(!strcmp("bsmdeStatus",table.Data()) || !strcmp("bsmdpStatus",table.Data())) draw((smdStatus_st*) dbHandler->getDbTable()->GetTable()); 
    if(!strcmp("bsmdeGain",table.Data()) || !strcmp("bsmdpGain",table.Data()))     draw((smdGain_st*) dbHandler->getDbTable()->GetTable()); 
    ////////////////////////////////////////////////////////////////
    if(!strcmp("bemcTriggerPed",table.Data()))    draw((emcTriggerPed_st*) dbHandler->getDbTable()->GetTable()); 
    if(!strcmp("bemcTriggerStatus",table.Data())) draw((emcTriggerStatus_st*) dbHandler->getDbTable()->GetTable()); 
    if(!strcmp("bemcTriggerLUT",table.Data()))    draw((emcTriggerLUT_st*) dbHandler->getDbTable()->GetTable()); 

    cout <<"Type:\n";
    cout <<"   next()          for the next plot in the list\n";
    cout <<"   previous()      for the previous plot in the list\n";
    cout <<"   plot(i)         for the i-entry in the list\n";
    cout <<"   stats()         for statistics\n";
}
void next()
{
    if(list.size()==0) return;
    index ++;
    if(index>=list.size()) index = 0;
    plot(index);
}

void previous()
{
    if(list.size()==0) return;
    index --;
    if(index<0) index = list.size()-1;
    plot(index);
}
