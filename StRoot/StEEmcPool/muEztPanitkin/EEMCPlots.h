#ifndef EEMCPlots_H
#define EEMCPlots_H

class TFile;
class TPad;
class TObjArray;

class EEqaSorter;
class StEEmcDb;

class EEMCPlots {

public:
    EEMCPlots(TObjArray *list = 0, const char *eemcDbDump = 0, const char *eemcPathIn = 0, const char *eemcPathOut = 0);
    ~EEMCPlots();

    //void init(unsigned int date, unsigned int time, const char *eemcDbDump, const char *eemcPathIn, const char *eemcPathOut);
    void resetHistograms();
    void saveHistograms(TFile *hfile);    
    void processEvent(	  char *rdr
        		, const unsigned char * dsm0inp = 0
        		, const unsigned short int  * dsm1inp = 0
        		, const unsigned short int  * dsm2inp = 0
        		, const unsigned short int  * dsm3inp = 0
			);

    // These are called from Pplots
    static void initHisto(TObjArray *list = 0, const char *eemcDbDump = 0, const char *eemcPathIn = 0, const char *eemcPathOut = 0);
    static void resetHisto();
    static void saveHisto(TFile *hfile);    
    static void fillHisto(char *rdr
        		, const unsigned char * dsm0inp = 0
        		, const unsigned short int  * dsm1inp = 0
        		, const unsigned short int  * dsm2inp = 0
        		, const unsigned short int  * dsm3inp = 0
			);

private:
    EEqaSorter *eeqa;
    StEEmcDb *eeDb;
};

#endif

