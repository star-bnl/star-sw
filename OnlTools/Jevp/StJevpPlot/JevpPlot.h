#ifndef _JEVPPLOT_H_
#define _JEVPPLOT_H_

#include <TROOT.h>
#include <TH1.h>
#include <TObject.h>
#include <TLegend.h>

// This class contains everything needed to draw a 
// single plot, whether it be a single histogram
// or a complex plot made from multiple histograms with 
// multiple colors/legends/whatever...

class PlotHisto : public TObject {
 public:
    TH1 *histo;

    char *legendText;
    char *legendArgs;
  
    PlotHisto(TH1 *hist = NULL);
    PlotHisto(PlotHisto &x);

    PlotHisto(const PlotHisto &x) {
	//LOG("JEFF", "Copy Constructor");
	histo = new TH1(*(x.histo));
	if(x.legendText) setLegText(x.legendText);
	if(x.legendArgs) setLegArgs(x.legendArgs);
    }

    // PlotHisto(TH1 *hist, char *legText=NULL, char *legArgs=NULL);
    void setLegText(const char *text);
    void setLegArgs(const char *text);

    virtual ~PlotHisto();

    ClassDef(PlotHisto, 1);
};

class JevpPlot : public TObject {
 public:
    // int run;
    TLegend *legend;
 
    int needsdata;

    int run;       
    int refid;         // reference name, if any
    char *refcomment;    // reference comment, if any


    // Plot specific root drawing options...
    int optstat;
    int logx;
    int logy;
    int optlogz;
    int palette;
    int gridx;
    int gridy;
    int lastUpdate;
    int nevts;

    double external_maxy;

    double legendx1;
    double legendy1;
    double legendx2;
    double legendy2;

    double getMaxY();
    void setMaxY(double ymax);
    void setRefComment(char *comment);
    void addHisto(PlotHisto *hist); 
    void addHisto(TH1 *roothist);
    void removeHisto(int i);
    PlotHisto *getHisto(int i);
    int isDataPresent();
    int nHistos();
    virtual const char *GetName() const { 
	return (const char *)myname; 
    };
    char *GetPlotName();
    void setPalette(int x) { palette = x; };
    void draw();
    void setLegend(double x1,double y1, double x2, double y2) {
	legendx1=x1;legendx2=x2;legendy1=y1;legendy2=y2;
    };
    void setOptStat(int x) { optstat = x; }
    void setDrawOpts(const char *opts);
    JevpPlot(TH1 *firsthisto=NULL);
    JevpPlot(JevpPlot &x);
    virtual ~JevpPlot();
  
    void reset();
    void setParent(char *parent);
    char *getParent();
  
    void addElement(TObject *element);
    void removeElement(TObject *element);
  
    TList histos;

    ULong_t Hash() const;
 private:
    char *xaxis_label;
    char *yaxis_label;

    char *parent;


    int nhistos;
    char *drawopts;

    char myname[100];

    TList elements;

    ClassDef(JevpPlot, 1);
};


#endif
