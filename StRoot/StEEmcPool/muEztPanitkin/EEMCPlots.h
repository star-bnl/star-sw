#ifndef EEMCPlots_H
#define EEMCPlots_H

class TFile;
class TPad;
class TObjArray;

class EEqaSorter;
class EEmcDb;

class EEMCPlots {

public:
    EEMCPlots(TObjArray *list = 0, const char *eemcDbDump = 0, const char *eemcPathIn = 0, const char *eemcPathOut = 0);
    ~EEMCPlots();

    void init(unsigned int date, unsigned int time, const char *eemcDbDump, const char *eemcPathIn, const char *eemcPathOut);
    void clear();
    void saveHistograms(TFile *hfile);    
    void processEvent(	  char *rdr
        		, const unsigned char * dsm0inp
        		, const unsigned short int  * dsm1inp
        		, const unsigned short int  * dsm2inp
        		, const unsigned short int  * dsm3inp
			);

    // These are called from Pplots
    static void initHisto(TObjArray *list = 0, const char *eemcDbDump = 0, const char *eemcPathIn = 0, const char *eemcPathOut = 0);
    static void resetHisto();
    static void saveHisto(TFile *hfile);    
    static void fillHisto(char *rdr
        		, const unsigned char * dsm0inp
        		, const unsigned short int  * dsm1inp
        		, const unsigned short int  * dsm2inp
        		, const unsigned short int  * dsm3inp
			);

private:
    EEqaSorter *eeqa;
    EEmcDb *eeDb;
};

#endif

