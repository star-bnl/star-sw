#include <algorithm>
#include <functional>
#include <sstream>

#include "utils.h"

#include "TAxis.h"
#include "TSystem.h"
#include "TClass.h"


using namespace std;

namespace utils {


void Apply(TH1* h, TF1* f)
{
   for (int ib=1; ib<=h->GetNbinsX(); ib++)
   {
      Double_t bc = h->GetBinContent(ib);
      bc = f->Eval(bc);
      //if ( isnan(bc) ) continue; // watch out!
      //cout << ib << " apply bc: " << bc;
      //if ( bc != bc ) continue; // watch out!
      if ( !isnormal(bc) ) continue; // watch out!
      //cout << " passed " << endl;

      //bc = isnan(bc) ? 0 : bc;
      h->SetBinContent(ib, bc);
   }
}


/** */
void PrintProgress(Int_t i, Int_t iMax)
{
   if (i==0) {
      printf("0%c           25%c           50%c           75%c"
             "          100%c\n", '%', '%', '%', '%', '%');
      printf("|-------------|-------------"
             "|-------------|-------------|\n");
      cout << flush;
   }
   // # of arrows per unit increment in i
   Double_t coef = 56.0/(Double_t)(iMax);
   //
   for (Int_t j=(Int_t)(coef*i); j!=(Int_t)(coef*(i+1)); ++j)
      cout << ">" << flush;
   //
   if (i==iMax-1)
      cout << ">" << endl;
}


void PrintTLorentzVector(const TLorentzVector &lv)
{
   // Print the TLorentz vector components as (x,y,z,t) and (P,eta,phi,E) representations
   Printf("(x,y,z,t)=(%f,%f,%f,%f) (P,eta,phi,E)=(%f,%f,%f,%f)",
      lv.Px(), lv.Py(), lv.Pz(), lv.E(), lv.P(), lv.Eta(), lv.Phi(), lv.E());
}


/** */
void ConvertToCumulative(TH1* h, Double_t norm)
{
   //h->Print("all");
   if (norm <= 0) { norm = 1.; }

   if (h->Integral()) 
      h->Scale(norm/h->Integral());

   Double_t integral = 0;

   for (int i=h->GetNbinsX(); i>=1; i--) {
      integral = h->Integral(1, i);
      h->SetBinContent(i, integral);
   }
   //h->Print("all");
}


/** */
TH1F* ConvertToCumulative2(const TH1* h, TH1F* hCumul, Bool_t sort)
{
   // Save all non-overflow bins in vector
   Int_t xfirst  = h->GetXaxis()->GetFirst();
   Int_t xlast   = h->GetXaxis()->GetLast();
   Int_t yfirst  = h->GetYaxis()->GetFirst();
   Int_t ylast   = h->GetYaxis()->GetLast();
   Int_t zfirst  = h->GetZaxis()->GetFirst();
   Int_t zlast   = h->GetZaxis()->GetLast();

   if (!hCumul)
      hCumul = new TH1F("hCumul", "hCumul", 100, 0, 1);

   multiset<Double_t, greater<Double_t> > hContent;

   Int_t    bin;
   Int_t    totBins = 0;
   Double_t totIntg = 0;
   Double_t value, error;

   for (Int_t binz=zfirst; binz<=zlast; binz++) {
      for (Int_t biny=yfirst; biny<=ylast; biny++) {
         for (Int_t binx=xfirst; binx<=xlast; binx++) {
            bin   = h->GetBin(binx,biny,binz);
            value = h->GetBinContent(bin);
            error = h->GetBinError(bin);

            // skip empty bins
            if (!value && !error) continue;

            hContent.insert((Double_t) value);
            totBins++;
            totIntg += value;
         }
      }
   }

   Int_t iBin = 0;

   multiset<Double_t>::iterator iter;
   multiset<Double_t>::iterator iB = hContent.begin();
   multiset<Double_t>::iterator iE = hContent.end();

   for (iter=iB; iter!=iE; ++iter, iBin++)
   {
      Double_t fracBins   = iBin/ (Double_t) totBins;
      Double_t fracIntg   = (*iter) / totIntg;
      Int_t    iCumulBinB = hCumul->FindBin(fracBins);

      for (Int_t iCumulBin=iCumulBinB; iCumulBin<=hCumul->GetXaxis()->GetLast(); iCumulBin++)
      {
         hCumul->AddBinContent(iCumulBin, fracIntg);
      }
   }

   return hCumul;
}


/** */
TH1* ConvertToProfile(const TH1* h, TH1* p, Bool_t weighted)
{
   //h->Print("all");
   //h->Scale(1./h->Integral());
   //Double_t err = 0;
   //Int_t    n = 0;

   //h->GetYaxis()->UnZoom();

   Double_t ymax = h->GetMaximum();
   Double_t ymin = h->GetMinimum();

   if (!p) {
      p = new TH1D("p", "p", 100, ymin-0.1*fabs(ymax-ymin), ymax+0.1*fabs(ymax-ymin));
   }

   //p->GetXaxis()->SetRangeUser(h->GetYaxis()->GetXmin(), h->GetYaxis()->GetXmax());
   //p->GetXaxis()->SetRangeUser(ymin-0.1*fabs(ymax-ymin), ymax+0.1*fabs(ymax-ymin));

   for (int i=1; i<=h->GetNbinsX(); i++) {
      Double_t bc = h->GetBinContent(i);
      Double_t be = h->GetBinError(i);

      if (weighted && bc != 0 && be != 0) {
         p->Fill(bc, 1/be/be);
         //p->Fill(bc);
      }

      if (!weighted && (bc != 0 || be != 0) ) {
      //if (!weighted) {
         p->Fill(bc);
      }

      //Double_t berr = h->GetBinError(i);
      //if (berr) {
      //   err += (berr*berr); // 1./(berr*berr)
      //   n++;
      //}
   }

   return p;
}


/** */
TH1F* ConvertToAbs(const TH1F* h)
{

   TH1F* hAbs = new TH1F(*h);

   for (int i=1; i<=h->GetNbinsX(); i++) {

      Double_t bc = h->GetBinContent(i);

      if (bc < 0) {
         hAbs->SetBinContent(i, TMath::Abs(bc));
      }
   }

   return hAbs;
}


/** */
TH1F* ConvertToMaxAbs(const TH1F* h1, const TH1F* h2)
{

   TH1F* hMaxAbs = new TH1F(*h1);

   for (int i=1; i<=h1->GetNbinsX(); i++) {

      Double_t bc1 = TMath::Abs(h1->GetBinContent(i));
      Double_t bc2 = TMath::Abs(h2->GetBinContent(i));

      if (bc1 > bc2) {
         hMaxAbs->SetBinContent(i, bc1);
      } else
         hMaxAbs->SetBinContent(i, bc2);
   }

   return hMaxAbs;
}


/** */
TH1* ConstructTH1C(string name, string title, TStyle *style,
   Int_t nbinsx, Double_t xlow, Double_t xup, Int_t nbinsy, Double_t ylow, Double_t yup,
   string opts)
{
   TH1* hist = new TH2C(name.c_str(), name.c_str(), nbinsx, xlow, xup, nbinsy, ylow, yup);
   hist->SetTitle(title.c_str());
   hist->SetOption(opts.c_str());

   return hist;
}


/** */
TH1* ConstructTH1CWithTGraphErrors(string name, string title, TStyle *style,
   Int_t nbinsx, Double_t xlow, Double_t xup, Int_t nbinsy, Double_t ylow, Double_t yup,
   string opts)
{
   TGraph *graph = new TGraphErrors();
   graph->SetName(string("gr" + name).c_str());
   
   if (style) ((TAttMarker*) style)->Copy(*graph);

   TH1* hist = new TH2C(name.c_str(), name.c_str(), nbinsx, xlow, xup, nbinsy, ylow, yup);
   hist->SetTitle(title.c_str());
   hist->SetOption(opts.c_str());
   hist->GetListOfFunctions()->Add(graph, "p");
   hist->GetListOfFunctions()->SetOwner(kTRUE);

   return hist;
}


/** */
TH1* ConstructTH1CWithTGraphErrorsMap(string name, string title,
   map<string, TStyle*> &sfx2styles,
   Int_t nbinsx, Double_t xlow, Double_t xup, Int_t nbinsy, Double_t ylow, Double_t yup,
   string opts)
{
   TH1* hist = new TH2C(name.c_str(), name.c_str(), nbinsx, xlow, xup, nbinsy, ylow, yup);
   hist->SetTitle(title.c_str());
   hist->SetOption(opts.c_str());

   map<string, TStyle*>::iterator iSfx2Style  = sfx2styles.begin();

   for ( ; iSfx2Style!=sfx2styles.end(); ++iSfx2Style)
   {
      string  sfx   = iSfx2Style->first;
      TStyle* style = iSfx2Style->second;

      TGraph *graph = new TGraphErrors();
      graph->SetName(string("gr" + name + sfx).c_str());
      ((TAttMarker*) style)->Copy(*graph);

      hist->GetListOfFunctions()->Add(graph, "p");
   }

   hist->GetListOfFunctions()->SetOwner(kTRUE);

   return hist;
}


/** */
TGraph* ExtractTGraph(TH1 &h, string sfx)
{
   string hName = h.GetName();
   //hName.erase(0, 1);
   hName = "gr" + hName + sfx;
   TGraph *graph = (TGraph*) h.GetListOfFunctions()->FindObject(hName.c_str());
   return graph;
}


/** */
void CopyBinContentError(const TH1* hFrom, TH1* hTo)
{
   Int_t nbinsx = hFrom->GetNbinsX();
   Int_t nbinsy = hFrom->GetNbinsY();
   Int_t nbinsz = hFrom->GetNbinsZ();

   if (hFrom->GetDimension() < 2) nbinsy = -1;
   if (hFrom->GetDimension() < 3) nbinsz = -1;

   for (Int_t binz=0;binz<=nbinsz+1;binz++) {
      for (Int_t biny=0;biny<=nbinsy+1;biny++) {
         for (Int_t binx=0;binx<=nbinsx+1;binx++) {

            Int_t bin = binx +(nbinsx+2)*(biny + (nbinsy+2)*binz);

            Double_t bc  = hFrom->GetBinContent(bin);
            Double_t be  = hFrom->GetBinError(bin);

            hTo->SetBinContent(bin, bc);
            hTo->SetBinError(bin, be);
         }
      }
   }

   hTo->SetEntries(hFrom->GetEntries());
}


/** */
TH1* CopyReversedBinContentError(const TH1* hFrom, TH1* hTo)
{
	if (!hFrom) {
		Warning("CopyReversedBins", "Invalid histogram provided");
		return 0;
	}

	if (!hTo) {
		Info("CopyReversedBins", "New histogram created. Ownership must be assumed");
	   hTo = new TH1(*hFrom);
	}

   Int_t nbinsx = hFrom->GetNbinsX();
   Int_t nbinsy = hFrom->GetNbinsY();
   Int_t nbinsz = hFrom->GetNbinsZ();

   if (hFrom->GetDimension() < 2) nbinsy = -1;
   if (hFrom->GetDimension() < 3) nbinsz = -1;

   for (Int_t binzFrom=0, binzTo=nbinsz+1; binzFrom<=nbinsz+1; binzFrom++, binzTo--) {
      for (Int_t binyFrom=0, binyTo=nbinsy+1; binyFrom<=nbinsy+1; binyFrom++, binyTo--) {
         for (Int_t binxFrom=0, binxTo=nbinsx+1; binxFrom<=nbinsx+1; binxFrom++, binxTo--) {

            Int_t binFrom = binxFrom + (nbinsx+2)*(binyFrom + (nbinsy+2)*binzFrom);
            Int_t binTo   = binxTo   + (nbinsx+2)*(binyTo   + (nbinsy+2)*binzTo);

            Double_t bc  = hFrom->GetBinContent(binFrom);
            Double_t be  = hFrom->GetBinError(binFrom);

            hTo->SetBinContent(binTo, bc);
            hTo->SetBinError(binTo, be);
         }
      }
   }

   hTo->SetEntries(hFrom->GetEntries());
	return hTo;
}


/** */
Double_t getIntegralLimits(TH1F* h, Double_t frac, Int_t &bmin, Int_t &bmax)
{
   Double_t mean, sum, integral;
   Int_t    bmean, i;

   mean  = h->GetMean(1);
   bmean = h->FindBin(mean);
   sum   = 0;
   integral = h->Integral();

   bmin = bmean;
   bmax = bmean;

   i = 1;

   while (kTRUE) {
      sum = h->Integral(bmin, bmax)/integral;
      if (sum >= frac) break;

      if (i%2 == 0 && bmin > 1) bmin--;
      if (i%2 != 0 && bmax < h->GetNbinsX()) bmax++;
      i++;
   }

   return sum;
}


/** */
TH2F* correctFit(TH2F* h2, TF1* f1)
{
   string hName = h2->GetName();
   hName += "_corr";
   //TH2F* h2c = new TH2F("correctTdcVsZ", "correctTdcVsZ", 100, -150, 150, 130, -50, 80);
   //TH2F* h2c = new TH2F(hName.c_str(), hName.c_str(), 100, min, max, 130, -50, 80);
   //sprintf(hName, "%s-hCotTimeResVsCh", hdiName);
   TH2F* h2c = (TH2F*) h2->Clone(hName.c_str());
   h2c->Reset();
   double ymin  = h2->GetYaxis()->GetXmin();
   double ymax  = h2->GetYaxis()->GetXmax();
   double ymean = h2->GetMean(2);
   h2c->GetYaxis()->SetLimits(ymin-ymean, ymax-ymean);

   for (int ix=1; ix<=h2->GetNbinsX(); ix++) {

      for (int iy=1; iy<=h2->GetNbinsY(); iy++) {

         double xbc = h2->GetXaxis()->GetBinCenter(ix);
         double ybc = h2->GetYaxis()->GetBinCenter(iy);

         int    ib  = h2->GetBin(ix, iy);
         double bc  = h2->GetBinContent(ib);
         //printf("%3d, %3d: %3f\n", ix, iy, bc);

         double y = f1->Eval(xbc);
         int ib2 = h2c->FindBin(xbc, ybc-y);
         h2c->SetBinContent(ib2, h2c->GetBinContent(ib2)+bc);
      }
   }

   return h2c;
}


/** */
void saveCanvas(TCanvas *canvas)
{
   TString hNameStr = canvas->GetName();
   canvas->SaveAs(hNameStr + ".png");
   canvas->SaveAs(hNameStr + ".eps");
}


/** */
TList* getFileList(TString fListName)
{
   TList *coll = new TList();

   ifstream infile(fListName.Data());
   string   fileId;

   // loop over the files
   while(infile >> fileId){
      //printf("file: %s\n", fileId.c_str());
      coll->Add(new TObjString(fileId.c_str()));
   }

   return coll;
}


/** */
void fluctuatePoisson(TH1* h, TRandom* rnd)
{
   //bool rndSet = false;
   TRandom tmpRnd;

   if (!rnd) { rnd = &tmpRnd; }

   for (int ix=0; ix<=h->GetNbinsX()+1; ix++) {
      //for (int iy=0; iy<=h->GetNbinsY()+1; iy++) {
         //for (int iz=0; iz<=h->GetNbinsZ()+1; iz++) {
            int iy = 0;
            int iz = 0;

            double bc = h->GetBinContent(ix, iy, iz);
            double bw = h->GetBinWidth(ix);
            bc = bc*bw;
            bc = rnd->Poisson(bc)/bw;
            //bc = bc/(bw/2.);
            h->SetBinContent(ix, iy, iz, bc);
            h->SetBinError(ix, iy, iz, TMath::Sqrt(bc));
         //}
      //}
   }

   //h->SetEntries(h->GetIntegral());

   //if (rndSet) delete rnd;
}


/** */
int FindFirstBinAbove(TH1F* h, double cl)
{
   Int_t nbins = h->GetXaxis()->GetNbins();
   for (Int_t bin=1;bin<=nbins;bin++) {
      if (h->GetBinContent(bin) > cl) return bin;
   }
   return -1;
}


/** Gets maximum bin in blurred hist */
Int_t FindMaximumBinEx(TH1F *h, int blur_radius)
{
   TH1F *blurred = (TH1F*)h->Clone("blurred");
   blurred->Reset();
   Int_t xfirst  = blurred->GetXaxis()->GetFirst();
   Int_t xlast   = blurred->GetXaxis()->GetLast();
   for (Int_t bin = xfirst; bin <= xlast; bin++) {
      Int_t start = TMath::Max(bin - blur_radius, xfirst);
      Int_t end   = TMath::Min(bin + blur_radius, xlast);
      Double_t value = 0;
      for (int i = start; i <= end; i++) {
         value += h->GetBinContent(i);
      }
      blurred->SetBinContent(bin, value);
   }
   Int_t max_bin = blurred->GetMaximumBin();
   delete blurred;
   return max_bin;
}


/** */
void BinGraph(TGraphErrors* gr, TH1* h)
{
   double x, y;
   double xe, ye;

   for (Int_t i=0; i<gr->GetN(); i++) {

      gr->GetPoint(i, x, y);

      xe = gr->GetErrorX(i);
      ye = gr->GetErrorY(i);

      Int_t    ib  = h->FindBin(x);
      Double_t ibc = h->GetBinContent(ib);
      Double_t ibe = h->GetBinError(ib);

      Double_t ibc_new;
      Double_t ibe_new;

      if (!ibc && !ibe) {
         ibc_new = y;
         ibe_new = ye;
      } else {
         Double_t w1 = 1., w2 = 1.;
         if (ye  > 0) w1 = 1./(ye*ye);
         if (ibe > 0) w2 = 1./(ibe*ibe);

         ibc_new = (w1*y + w2*ibc)/(w1 + w2);
         ibe_new = 1./TMath::Sqrt(w1 + w2);
      }

      //printf("i: %8d %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n", ib, ibc, ibe, x, y, ye, w1, w2, ibc_new, ibe_new);
      //printf("i: %8d %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n", ib, ibc, ibe, x, y, ye, ibc_new, ibe_new);

      h->SetBinContent(ib, ibc_new);
      h->SetBinError(ib, ibe_new);
   }
}


/** */
void BinGraphByFirstOnly(TGraphErrors* gr, TH1* h)
{
   double x, y;
   double xe, ye;

   for (Int_t i=0; i<gr->GetN(); i++)
   {
      gr->GetPoint(i, x, y);

      //if (fabs(x) > 3) continue;

      xe = gr->GetErrorX(i);
      ye = gr->GetErrorY(i);

      Int_t    ib  = h->FindBin(x);
      Double_t ibc = h->GetBinContent(ib);
      Double_t ibe = h->GetBinError(ib);

      Double_t ibc_new;
      Double_t ibe_new;

      if (!ibc && !ibe) {
         ibc_new = y;
         ibe_new = ye;
      } else { // the bin has been filled already, so skip all other values for this x/ib
         continue;
      }

      //printf("i: %8d %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n", ib, ibc, ibe, x, y, ye, w1, w2, ibc_new, ibe_new);
      //printf("i: %8d %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n", ib, ibc, ibe, x, y, ye, ibc_new, ibe_new);

      h->SetBinContent(ib, ibc_new);
      h->SetBinError(ib, ibe_new);
   }
}


/** */
void BinGraphsByMeasId(TList* grList, TH1* h, Bool_t norm)
{
   TIter next(grList);

   while ( TGraphErrors *gr = (TGraphErrors*) next() ) {

      Double_t x, y, y_first = 1;
      Double_t xe, ye;

      for (Int_t i=0; i<gr->GetN(); i++) {

         gr->GetPoint(i, x, y);

         xe = gr->GetErrorX(i);
         ye = gr->GetErrorY(i);

         // Get the first value to be used for normalization
         if (norm && i == 0)
            y_first = y;

         // normalize values
         y  /= y_first;
         ye /= y_first;

         Int_t    ib  = i + 1;
         Double_t ibc = h->GetBinContent(ib);
         Double_t ibe = h->GetBinError(ib);

         Double_t ibc_new;
         Double_t ibe_new;

         // the bin is empty
         if (!ibc && !ibe) {
            ibc_new = y;
            ibe_new = ye;
         } else { // the bin is NOT empty
            Double_t w1 = 1., w2 = 1.;
            if (ye  > 0) w1 = 1./(ye*ye);
            if (ibe > 0) w2 = 1./(ibe*ibe);

            ibc_new = (w1*y + w2*ibc)/(w1 + w2);
            ibe_new = 1./TMath::Sqrt(w1 + w2);
         }

         //printf("i: %8d %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n", ib, ibc, ibe, x, y, ye, w1, w2, ibc_new, ibe_new);
         //printf("i: %8d %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n", ib, ibc, ibe, x, y, ye, ibc_new, ibe_new);

         h->SetBinContent(ib, ibc_new);
         h->SetBinError(ib, ibe_new);
      }
   }
}


/** */
ValErrPair* FindValErrY(TGraph* gr, Double_t xval, Int_t *pindex)
{
   if (!gr) return 0;

   double x, y;
   double xe, ye;

   ValErrPair *valerr = 0;

   for (Int_t i=0; i<gr->GetN(); i++) {

      gr->GetPoint(i, x, y);
      xe = gr->GetErrorX(i);
      ye = gr->GetErrorY(i);

      if (x == xval) {
         valerr = new ValErrPair();
         valerr->first  = y;
         valerr->second = ye;

         if (pindex) *pindex = i;

         return valerr;
      }
   }

   // nothing found
   return valerr;
}


/** */
TGraph* SubGraph(TGraph* gr, Double_t xlo, Double_t xhi)
{
   TGraph* subgr = new TGraph(0);

   if (!gr) return subgr;
   if (xlo == xhi || xlo > xhi) return (TGraph*) gr->Clone();

   Double_t x, y;
   UInt_t iPoint = 0;

   for (Int_t i=0; i<gr->GetN(); ++i) {
      gr->GetPoint(i, x, y);
      if ( x < xlo || x > xhi ) continue;

      subgr->SetPoint(iPoint, x, y);
      iPoint++;
   }

   return subgr;
}


/** */
void RemoveOutliers(TGraph* gr, Int_t axis, Float_t sigma)
{
   Double_t mean = gr->GetMean(axis);
   Double_t rms  = gr->GetRMS(axis);

   //printf("mean, rms: %f, %f\n", mean, rms);

   Double_t x, y;
   //vector<Int_t> toberemoved;

   for (Int_t i=0; i<gr->GetN(); i++) {

      gr->GetPoint(i, x, y);
      Double_t delta = 0;

      if (axis == 1 ) delta = fabs(mean - x);
      if (axis == 2 ) delta = fabs(mean - y);

      //printf("delta: %f\n", delta);

      if (delta > sigma*rms) {
         gr->RemovePoint(i);//toberemoved.push_back(i);
         //printf("point %d removed\n", i);
         i--;
      }
   }
}


/** */
void RemoveOutliers(TH1* h, Float_t sigmax)
{
   Double_t meanx = h->GetMean(1);
   Double_t rmsx  = h->GetRMS(1);

   //printf("meanx, rmsx: %f, %f\n", meanx, rmsx);

   for (Int_t ibx=1; ibx<=h->GetNbinsX(); ibx++) {

      Double_t bcentr = h->GetBinCenter(ibx);
      Double_t delta = fabs(meanx - bcentr);

      //printf("delta: %f\n", delta);

      if (delta > sigmax*rmsx) {
         h->SetBinContent(ibx, 0);
         h->SetBinError(ibx, 0);
      }
   }
}


/** */
void SubtractMinimum(TGraph* gr, Int_t axis)
{
   Double_t x, y;
   Double_t xmin, ymin, xmax, ymax;

   gr->ComputeRange(xmin, ymin, xmax, ymax);

   for (Int_t i=0; i<gr->GetN(); ++i) {
      gr->GetPoint(i, x, y);

      if (axis == 1)
         gr->SetPoint(i, x-xmin, y);

      if (axis == 2)
         gr->SetPoint(i, x, y-ymin);
   }
}


/** */
void UpdateLimits(TH1* h, Int_t axis, Bool_t incErr)
{
   Int_t bin, binx, biny, binz;

   Int_t xfirst  = h->GetXaxis()->GetFirst();
   Int_t xlast   = h->GetXaxis()->GetLast();
   Int_t yfirst  = h->GetYaxis()->GetFirst();
   Int_t ylast   = h->GetYaxis()->GetLast();
   Int_t zfirst  = h->GetZaxis()->GetFirst();
   Int_t zlast   = h->GetZaxis()->GetLast();

   Double_t value, error, valPlusErr, valMinusErr;
   Double_t max = -FLT_MAX;
   Double_t min =  FLT_MAX;

   for (binz=zfirst; binz<=zlast; binz++) {
      for (biny=yfirst; biny<=ylast; biny++) {
         for (binx=xfirst; binx<=xlast; binx++) {
            bin   = h->GetBin(binx, biny, binz);
            value = h->GetBinContent(bin);
            error = h->GetBinError(bin);

            // skip empty bins
            if (value == 0 && error == 0) continue;

            if (incErr) { valPlusErr = value + error; valMinusErr = value - error; }
            else        { valPlusErr = value; valMinusErr = value; }

            if (valPlusErr  > max) max = valPlusErr;
            if (valMinusErr < min) min = valMinusErr;
            //Info("utils::UpdateLimits", "bin, value, error, max, min: %d, %f, %f, %f, %f\n", bin, value, error, max, min);
         }
      }
   }

   if (h->GetDimension() == 1) {
      if (max <= min) return;
      Double_t margin = fabs(max - min)*0.1;
      h->GetYaxis()->SetRangeUser(min-margin, max+margin);
      //h->GetYaxis()->SetLimits(min, max); // this does not work for some reason
   } else if (h->GetDimension() == 2) {
      //h->GetZaxis()->SetLimits(min, max);
      h->GetZaxis()->SetRangeUser(min, max);
   } else {
      Warning("utils::UpdateLimits", "Expected a TH1 or TH2 histogram");
   }
}


/** */
void UpdateLimitsFromGraphs(TH1* h, Int_t axis, Bool_t incErr)
{
   //gSystem->Warning("utils::UpdateLimitsFromGraphs", "Graphs found in histogram %s", h->GetName());

   Double_t xmin, ymin, xmax, ymax;
   Double_t xminAll =  DBL_MAX, yminAll =  DBL_MAX;
   Double_t xmaxAll = -DBL_MAX, ymaxAll = -DBL_MAX;

   TList* list = h->GetListOfFunctions();

   if (list->GetSize() <= 0) {
      gSystem->Warning("utils::UpdateLimitsFromGraphs", "No graphs found in histogram %s", h->GetName());
      return;
   }

   TIter next(list);

   while (TObject *o = next())
   {
      if ( !((TClass*) o->IsA())->InheritsFrom("TGraph") ) continue;

      if ( ((TGraph*) o)->GetN() <= 0 ) continue;

      ((TGraph*) o)->ComputeRange(xmin, ymin, xmax, ymax);

      if (xmin < xminAll) xminAll = xmin;
      if (xmax > xmaxAll) xmaxAll = xmax;
      if (ymin < yminAll) yminAll = ymin;
      if (ymax > ymaxAll) ymaxAll = ymax;

      //printf("xmin, ymin, xmax, ymax, xminAll, yminAll, xmaxAll, ymaxAll: %f, %f, %f, %f, %f, %f, %f, %f\n",
      //        xmin, ymin, xmax, ymax, xminAll, yminAll, xmaxAll, ymaxAll);
   }

   if (axis == 1) {
      if (xmaxAll <= xminAll) return;
      h->GetXaxis()->SetLimits(xminAll, xmaxAll); // why don't use SetRangeUser???
      //h->GetXaxis()->SetRangeUser(xminAll, xmaxAll);
   } else if (axis == 2) {
      if (ymaxAll <= yminAll) return;
      Double_t margin = fabs(ymaxAll - yminAll)*0.1;
      h->GetYaxis()->SetLimits(yminAll-margin, ymaxAll+margin); // why don't use SetRangeUser???
      //h->GetYaxis()->SetRangeUser(yminAll, ymaxAll);
   } else {
      if (xmaxAll > xminAll) {
         Double_t margin = fabs(xmaxAll - xminAll)*0.1;
         h->GetXaxis()->SetLimits(xminAll-margin, xmaxAll+margin);
      }

      if (ymaxAll > yminAll) {
         Double_t margin = fabs(ymaxAll - yminAll)*0.1;
         h->GetYaxis()->SetLimits(yminAll-margin, ymaxAll+margin);
      }

      //h->GetXaxis()->SetRangeUser(xminAll, xmaxAll);
      //h->GetYaxis()->SetRangeUser(yminAll, ymaxAll);
   }
}


/** */
void RebinIntegerAxis(TH1* h, Int_t axis)
{
   //if (axis == 1) {
      Double_t xmin = h->GetXaxis()->GetXmin();
      Double_t xmax = h->GetXaxis()->GetXmax();

      h->SetBins((Int_t) (xmax-xmin), xmin, xmax);

   //} else if (axis == 2) {
   //} else {
   //}
}


/** */
void MergeGraphs(TGraph* graph, TCollection* li)
{
   // Adds all graphs from the collection to this graph.
   // Returns the total number of poins in the result or -1 in case of an error.
   if (!graph) {
      Error("MergeGraphs", "Cannot merge graphs. Final graph not provided");
      return;
   }

   Bool_t isTGraphErrors = ((TClass*) graph->IsA())->InheritsFrom("TGraphErrors") ? kTRUE : kFALSE;

   TIter next(li);

   while (TObject *o = next())
   {
      TGraph *g;

      if ( ((TClass*) o->IsA())->InheritsFrom("TGraph") ) {
         g = (TGraph*) o;
      } else if ( ((TClass*) o->IsA())->InheritsFrom("TGraphErrors") ) {
         g = (TGraphErrors*) o;
      } else {
         Warning("MergeGraphs", "Non TGraph object found in list. Skipping...");
         continue;
      }

      Double_t x,  y;
      Double_t xe, ye;

      for (Int_t i=0; i<g->GetN(); i++) {

         g->GetPoint(i, x, y);
         xe = g->GetErrorX(i);
         ye = g->GetErrorY(i);

         Int_t iPoint = graph->GetN();

         graph->SetPoint(iPoint, x, y);

         if (isTGraphErrors && xe>=0 && ye>=0) {
            ((TGraphErrors*) graph)->SetPointError(iPoint, xe, ye);
         }
      }
   }
}


/** */
void MergeGraphs(TGraph *graph, TGraph *graphAdd)
{
   if (!graph) return;
   if (!graphAdd) return;

   Bool_t isTGraphErrors = ((TClass*) graph->IsA())->InheritsFrom("TGraphErrors") ? kTRUE : kFALSE;

   Double_t x,  y;
   Double_t xe, ye;

   for (Int_t i=0; i<graphAdd->GetN(); i++)
   {
      graphAdd->GetPoint(i, x, y);
      xe = graphAdd->GetErrorX(i);
      ye = graphAdd->GetErrorY(i);
      ValErrPair vePair1(y, ye);

      Int_t pindex;
      ValErrPair *vePair2 = FindValErrY(graph, x, &pindex);

      if ( vePair2 ) { // replace y values for given x
         ValErrPair avrgPair = CalcWeightedAvrgErr(*vePair2, vePair1);
         graph->SetPoint(pindex, x, avrgPair.first);
         if (isTGraphErrors) ((TGraphErrors*) graph)->SetPointError(pindex, xe, avrgPair.second);
      } else { // add new y values from graphAdd
         Int_t iPoint = graph->GetN();
         graph->SetPoint(iPoint, x, y);
         if (isTGraphErrors) ((TGraphErrors*) graph)->SetPointError(iPoint, xe, ye);
      }
   }
}


/** */
void AppendToGraph(TGraph *gr, Double_t x, Double_t y, Double_t xe, Double_t ye)
{
   if (!gr) return;

   UInt_t iPoint = gr->GetN();
   gr->SetPoint(iPoint, x, y);

   if ( ((TClass*) gr->IsA())->InheritsFrom("TGraphErrors"))
      ((TGraphErrors*) gr)->SetPointError(iPoint, xe, ye);
}


/** */
void AppendToTGraph(TH1 &h, Double_t x, Double_t y, Double_t xe, Double_t ye, string sfx)
{
   TGraph *gr = ExtractTGraph(h, sfx);

   if (!gr) return;

   UInt_t iPoint = gr->GetN();
   gr->SetPoint(iPoint, x, y);

   if ( ((TClass*) gr->IsA())->InheritsFrom("TGraphErrors"))
      ((TGraphErrors*) gr)->SetPointError(iPoint, xe, ye);
}


/** */
void PrintNice(TH1* h, FILE *f)
{
   TGraphErrors *gr = new TGraphErrors(h);
	PrintNice(gr, f);
}


/** */
void PrintNice(TGraph* gr, FILE *f)
{
   setbuf(f, NULL);
   fprintf(f, "%s\n", gr->GetName());

   fprintf(f, "%8s, %8s, %8s, %8s, %8s\n", "n", "x", "y", "x_err", "y_err");

   Double_t x, y, xe, ye;

   for (Int_t i=0; i<gr->GetN(); i++) {
      gr->GetPoint(i, x, y);
      xe = gr->GetErrorX(i);
      ye = gr->GetErrorY(i);
      fprintf(f, "%8d, %8.3f, %8.3f, %8.3f, %8.3f\n", i, x, y, xe, ye);
   }
}


/** */
void PrintNiceToFile(TH1* h, string fileName)
{

   if (fileName == "") {
      fileName  = h->GetName();
      fileName += ".txt";
   }

   FILE *f = fopen(fileName.c_str(), "w");

   PrintNice(h, f);
}


/** */
void PrintNiceToFile(TGraph* gr, string fileName)
{
   if (fileName == "") {
      fileName  = gr->GetName();
      fileName += ".txt";
   }

   FILE *f = fopen(fileName.c_str(), "w");

   PrintNice(gr, f);
}


/** */
Double_t GetNonEmptyMaximum(TH1* h, Double_t maxval)
{
   //  Return minimum value smaller than maxval of bins in the range,
   //  unless the value has been overridden by TH1::SetMinimum,
   //  in which case it returns that value. (This happens, for example,
   //  when the histogram is drawn and the y or z axis limits are changed
   //
   //  To get the minimum value of bins in the histogram regardless of
   //  whether the value has been overridden, use
   //     h->GetBinContent(h->GetMinimumBin())

   //if (h->fMaximum != -1111) return h->fMaximum;

   Int_t bin, binx, biny, binz;

   Int_t xfirst  = h->GetXaxis()->GetFirst();
   Int_t xlast   = h->GetXaxis()->GetLast();
   Int_t yfirst  = h->GetYaxis()->GetFirst();
   Int_t ylast   = h->GetYaxis()->GetLast();
   Int_t zfirst  = h->GetZaxis()->GetFirst();
   Int_t zlast   = h->GetZaxis()->GetLast();

   Double_t maximum = -FLT_MAX, value, error;

   if (h->GetEntries()) {
      for (binz=zfirst;binz<=zlast;binz++) {
         for (biny=yfirst;biny<=ylast;biny++) {
            for (binx=xfirst;binx<=xlast;binx++) {
               bin   = h->GetBin(binx,biny,binz);
               value = h->GetBinContent(bin);
               error = h->GetBinError(bin);

               if (value+error > maximum && value+error < maxval && !(value == 0 && error == 0) )
               //if (value > maximum && value < maxval )
                  maximum = value+error;
            }
         }
      }
   }

   //if (h->GetDimension() >= 2) return maximum;

   // If the histogram is one dimensional and has graph objects update the maximum
   Double_t xmin, ymin, xmax, ymax;

   TList* list = h->GetListOfFunctions();
   TIter  next(list);

   //while ( TGraph *graph = (TGraph*) next() ) {
   while ( TObject *graph = (TObject*) next() ) {
      if ( ! ( (TClass*) graph->IsA() )->InheritsFrom("TGraph") ) continue;
      if ( ((TGraph*) graph)->GetN() <= 0) continue;
      //graph->ComputeRange(xmin, ymin, xmax, ymax);
      ((TGraph*) graph)->ComputeRange(xmin, ymin, xmax, ymax);
      if (ymax > maximum) maximum = ymax;
   }

   return maximum;
}


/** */
Double_t GetNonEmptyMinimum(TH1* h, Double_t minval)
{
   //  Return minimum value smaller than maxval of bins in the range,
   //  unless the value has been overridden by TH1::SetMinimum,
   //  in which case it returns that value. (This happens, for example,
   //  when the histogram is drawn and the y or z axis limits are changed
   //
   //  To get the minimum value of bins in the histogram regardless of
   //  whether the value has been overridden, use
   //     h->GetBinContent(h->GetMinimumBin())

   //if (h->fMinimum != -1111) return h->fMinimum;

   Int_t bin, binx, biny, binz;

   Int_t xfirst  = h->GetXaxis()->GetFirst();
   Int_t xlast   = h->GetXaxis()->GetLast();
   Int_t yfirst  = h->GetYaxis()->GetFirst();
   Int_t ylast   = h->GetYaxis()->GetLast();
   Int_t zfirst  = h->GetZaxis()->GetFirst();
   Int_t zlast   = h->GetZaxis()->GetLast();

   Double_t minimum = FLT_MAX, value, error;

   if (h->GetEntries()) {
      for (binz=zfirst;binz<=zlast;binz++) {
         for (biny=yfirst;biny<=ylast;biny++) {
            for (binx=xfirst;binx<=xlast;binx++) {
               bin   = h->GetBin(binx,biny,binz);
               value = h->GetBinContent(bin);
               error = h->GetBinError(bin);

               if (value-error < minimum && value-error > minval && !(value == 0 && error == 0) )
               //if (value < minimum && value > minval )
                  minimum = value-error;
            }
         }
      }
   }

   //if (h->GetDimension() >= 2) return minimum;

   // If the histogram is one dimensional and has graph objects update the minimum
   Double_t xmin, ymin, xmax, ymax;

   TList* list = h->GetListOfFunctions();
   TIter  next(list);

   //while ( TGraph *graph = (TGraph*) next() ) {
   while ( TObject *graph = (TObject*) next() ) {
      if ( ! ( (TClass*) graph->IsA() )->InheritsFrom("TGraph") ) continue;
      if ( ((TGraph*) graph)->GetN() <= 0) continue;
      ((TGraph*) graph)->ComputeRange(xmin, ymin, xmax, ymax);
      if (ymin < minimum) minimum = ymin;
   }

   return minimum;
}


/** */
Double_t GetNonEmptyMaximum(THStack* hs, Option_t *option)
{
   //  returns the minimum of all added histograms
   //  returns the minimum of all histograms if option "nostack".

   TString opt = option;
   opt.ToLower();
   //Bool_t lerr = kFALSE;
   //if (opt.Contains("e")) lerr = kTRUE;
   Double_t themax = -FLT_MAX;
   if (!hs->GetHists()) return 0;
   Int_t nhists = hs->GetHists()->GetSize();
   TH1 *h;

   if (!opt.Contains("nostack")) {
      //BuildStack();
      gSystem->Warning("   utils::GetNonEmptyMaximum()", "stack option not supported. Use THStack class");
      h = (TH1*) hs->GetStack()->At(nhists-1);
      //themax = h->GetMaximum();
      themax = GetNonEmptyMaximum(h);
   } else {
      for (Int_t i=0;i<nhists;i++) {
         h = (TH1*) hs->GetHists()->At(i);
         //them = h->GetMaximum();
         Double_t them = GetNonEmptyMaximum(h);
         //if (them <= 0 && gPad && gPad->GetLogy()) them = h->GetMaximum(0);
         if (them > themax) themax = them;
         //printf("them, themax: %g, %g\n", them, themax);
      }
   }

   //if (lerr) {
   //   for (Int_t i=0;i<nhists;i++) {
   //      h = (TH1*) hs->GetHists()->At(i);
   //      first = h->GetXaxis()->GetFirst();
   //      last  = h->GetXaxis()->GetLast();
   //      for (Int_t j=first; j<=last;j++) {
   //          e1     = h->GetBinError(j);
   //          c1     = h->GetBinContent(j);
   //          themax = TMath::Max(themax,c1+e1);
   //      }
   //   }
   //}

   return themax;
}


/** */
Double_t GetNonEmptyMinimum(THStack* hs, Option_t *option)
{
   //  returns the minimum of all added histograms
   //  returns the minimum of all histograms if option "nostack".

   TString opt = option;
   opt.ToLower();
   //Bool_t lerr = kFALSE;
   //if (opt.Contains("e")) lerr = kTRUE;
   Double_t themin = FLT_MAX;
   if (!hs->GetHists()) return themin;
   Int_t nhists = hs->GetHists()->GetSize();
   TH1 *h;

   if (!opt.Contains("nostack")) {
      //BuildStack();
      gSystem->Warning("   utils::GetNonEmptyMinimum()", "stack option not supported. Use THStack class");
      h = (TH1*) hs->GetStack()->At(nhists-1);
      //themin = h->GetMinimum();
      themin = GetNonEmptyMinimum(h);
   } else {
      for (Int_t i=0;i<nhists;i++) {
         h = (TH1*) hs->GetHists()->At(i);
         //them = h->GetMinimum();
         Double_t them = GetNonEmptyMinimum(h);
         //if (them <= 0 && gPad && gPad->GetLogy()) them = h->GetMinimum(0);
         //if (them <= 0 && gPad && gPad->GetLogy()) them = GetNonEmptyMinimum(h, 0);
         if (them < themin) themin = them;
         //printf("them, themin: %g, %g\n", them, themin);
      }
   }

   //Int_t first,last;
   //
   //if (lerr) {
   //   for (Int_t i=0;i<nhists;i++) {
   //      h = (TH1*) hs->GetHists()->At(i);
   //      first = h->GetXaxis()->GetFirst();
   //      last  = h->GetXaxis()->GetLast();
   //      for (Int_t j=first; j<=last;j++) {
   //          e1     = h->GetBinError(j);
   //          c1     = h->GetBinContent(j);
   //          themin = TMath::Min(themin,c1-e1);
   //      }
   //   }
   //}

   return themin;
}


/** */
Double_t GetNonEmptyFraction(const TH1* h)
{
   Int_t nbinsx = h->GetNbinsX() < 1 ? 1 : h->GetNbinsX();
   Int_t nbinsy = h->GetNbinsY() < 1 ? 1 : h->GetNbinsY();
   Int_t nbinsz = h->GetNbinsZ() < 1 ? 1 : h->GetNbinsZ();

   Int_t nonEmptyBins = 0;
   Int_t totalBins;

   totalBins  = nbinsx;
   totalBins *= nbinsy;
   totalBins *= nbinsz;

   for (int ibz=1; ibz<=nbinsz; ibz++) {
      for (int iby=1; iby<=nbinsy; iby++) {
         for (int ibx=1; ibx<=nbinsx; ibx++)
         {
            if (h->GetBinContent(ibx, iby, ibz))
               nonEmptyBins++;
         }
      }
   }

   return ((Double_t) nonEmptyBins) / ((Double_t) totalBins);
}


/** 
 * Calculates the weighted average of two histograms ignorging bins with invalid content.
 */
TH1* AverageIgnoreEmptyBins(const TH1* h1, const TH1* h2, TH1* h)
{
   if (!h) h = new TH1(*h1);

   h->Reset();

   Int_t nbinsx = h->GetNbinsX();
   Int_t nbinsy = h->GetNbinsY();
   Int_t nbinsz = h->GetNbinsZ();
   Int_t bin, binx, biny, binz;

   if (h->GetDimension() < 2) nbinsy = -1;
   if (h->GetDimension() < 3) nbinsz = -1;

   for (binz=0;binz<=nbinsz+1;binz++) {
      for (biny=0;biny<=nbinsy+1;biny++) {
         for (binx=0;binx<=nbinsx+1;binx++) {

            bin = binx + (nbinsx+2)*(biny + (nbinsy+2)*binz);

            //special case where histograms have the kIsAverage bit set
            Double_t y1 = h1->GetBinContent(bin);
            Double_t e1 = h1->GetBinError(bin);
            Double_t y2 = h2->GetBinContent(bin);
            Double_t e2 = h2->GetBinError(bin);

            ValErrPair result = CalcWeightedAvrgErr( ValErrPair(y1, e1), ValErrPair(y2, e2) );

				if (result.second <= 0) continue;

            h->SetBinContent(bin, result.first);
            h->SetBinError(bin, result.second);
         }
      }
   }

   return h;
}


/** */
TH1* Divide(TH1* h1, TH1* h2, Double_t r12, TH1* h)
{
   if (!h) h = new TH1(*h1);

   h->Reset();

   Int_t nbinsx = h->GetNbinsX();
   Int_t nbinsy = h->GetNbinsY();
   Int_t nbinsz = h->GetNbinsZ();
   Int_t bin, binx, biny, binz;

   if (h->GetDimension() < 2) nbinsy = -1;
   if (h->GetDimension() < 3) nbinsz = -1;

   for (binz=0;binz<=nbinsz+1;binz++) {
      for (biny=0;biny<=nbinsy+1;biny++) {
         for (binx=0;binx<=nbinsx+1;binx++) {

            bin = binx + (nbinsx+2)*(biny + (nbinsy+2)*binz);

            //special case where histograms have the kIsAverage bit set
            Double_t y1 = h1->GetBinContent(bin);
            Double_t e1 = h1->GetBinError(bin);

            Double_t y2 = h2->GetBinContent(bin);
            Double_t e2 = h2->GetBinError(bin);

            //if ( y1 == 0 || y2 == 0 || (y1==0 && e1<=0) || (y2<=0 && e2<=0) ) continue;
            if ( y1 == 0 || y2 == 0 ) continue;

            Double_t bc = y1/y2;

            Double_t re1 = e1/y1;
            Double_t re2 = e2/y2;

            Double_t re1_2 = re1 * re1;
            Double_t re2_2 = re2 * re2;

            Double_t re_2 = re1_2 + re2_2 - 2*re1*re2*r12;

            h->SetBinContent(bin, bc);
            h->SetBinError(bin, bc*sqrt(re_2));
         }
      }
   }

   return h;
}


// Description : calculate weighted mean
// Input       : Double_t A[N], Double_t dA[N], int NDAT
// Return      : weighted mean
Double_t WeightedMean(Double_t *A, Double_t *dA, int NDAT)
{
   Double_t sum1 = 0;
	Double_t sum2 = 0;
   Double_t dA2  = 0;
 
   for (int i=0; i<NDAT; i++) {
      if (dA[i]) {  // skip dA=0 data
         dA2 = dA[i]*dA[i];
         sum1 += A[i]/dA2 ;
         sum2 += 1/dA2 ;
      }
   }
 
   return dA2 == 0 ? -1 : sum1/sum2;
}


// Description : calculate weighted mean error. A[i] is skipped if dA[i]=0.
// Input       : Double_t dA[N], int NDAT
// Return      : weighted mean error
Double_t WeightedMeanError(Double_t *dA, int NDAT)
{
   Double_t sum = 0;
   Double_t dA2 = 0;
 
   for ( int i=0 ; i<NDAT ; i++ ) {
      if (dA[i]){
         dA2  = dA[i]*dA[i];
         sum += 1/dA2 ;
      }
   }
 
   return sum == 0 ? -1 : sqrt(1/sum);
}


// Description : call weighted mean and error
// Input       : Double_t A[N], Double_t dA[N], Double_t Ave, int NDAT
// Return      : Ave, dAve
void CalcWeightedMean(Double_t *A, Double_t *dA, int NDAT, Double_t &Ave, Double_t &dAve)
{
   Ave  = WeightedMean(A, dA, NDAT);
   dAve = WeightedMeanError(dA, NDAT);
}


/** */
ValErrPair CalcWeightedAvrgErr(const ValErrSet &valerrs)
{
   ValErrPair    avrgResult;
   ValErrSetIter iValErr = valerrs.begin();

	Int_t n = 0;
	vector<Double_t> vals;
	vector<Double_t> errs;

   for ( ; iValErr != valerrs.end(); ++iValErr) {
      if (iValErr->second < 0) continue;
	   vals.push_back(iValErr->first);
	   errs.push_back(iValErr->second);
	   n++;
	}

   avrgResult.first  = WeightedMean(&vals[0], &errs[0], n);
   avrgResult.second = WeightedMeanError(&errs[0], n);

	return avrgResult;
}


/** */
ValErrPair CalcWeightedAvrgErr(const ValErrPair ve1, const ValErrPair ve2)
{
   ValErrPair result(0, -1);

   if (ve1.second <= 0 && ve2.second <= 0) return result;
   if (ve1.second <= 0) return ve2;
   if (ve2.second <= 0) return ve1;

   Double_t w1 = 1./ve1.second/ve1.second;
   Double_t w2 = 1./ve2.second/ve2.second;

   result.first  = (ve1.first*w1 + ve2.first*w2)/ (w1 + w2);
   result.second = 1./sqrt(w1 + w2);

	return result;
}


// Description : Calculates error propagation of x/y for (x,dx) and (y,dy)
//             :
// Input       : Double_t x, Double_t y, Double_t dx, Double_t dy
// Return      : error propagation of x/y
Double_t CalcDivisionError(Double_t x, Double_t y, Double_t dx, Double_t dy)
{
   if (x*y) {
      return x/y * sqrt( dx*dx/x/x + dy*dy/y/y );
   } else {
      return 0;
   }
}


/** */
ValErrPair CalcDivision(ValErrPair ve1, ValErrPair ve2, Double_t r12)
{
   ValErrPair result(0, -1);

   if (ve1.first == 0 || ve2.first == 0) return result;

   result.first  = ve1.first / ve2.first;
   Double_t re1  = ve1.second/ve1.first;
   Double_t re2  = ve2.second/ve2.first;
   Double_t re   = sqrt(re1*re1 + re2*re2 - 2*re1*re2*r12);
   result.second = re * result.first;

   return result;
}


// Description : calculate quadratic error of x/y
// Input       : Double_t x, Double_t y, Double_t dx, Double_t dy
// Return      : Double_t quadratic error of x/y
Double_t QuadErrorDiv(Double_t x, Double_t y, Double_t dx, Double_t dy)
{
  return y*x ? x/y * TMath::Sqrt(dx*dx/x/x + dy*dy/y/y) : 0;
}


// Description : calculate quadratic sum
// Input       : Double_t dx, Double_t dy
// Return      : Double_t quadratic error sum of x+y or x-y
//
Double_t QuadErrorSum(Double_t dx, Double_t dy)
{
   return TMath::Sqrt(dx*dx + dy*dy);
}


/** */
Double_t PackDecimal(ValErrPair ve)
{
   Int_t integer = (Int_t) (ve.first  * 1e8);
   Int_t decimal = (Int_t) (ve.second * 1e8);
   //Double_t decimal = ve.second;

   integer = integer >  999999999 ?  999999999 : integer;
   integer = integer < -999999999 ? -999999999 : integer;

   decimal = (decimal > 999999999 || decimal < 0) ? 999999999 : decimal;

   Double_t result = integer + ((Double_t) decimal) * 1e-9;

   printf("Pack: %f, %f, %f\n", ve.first, ve.second, result);

   return (Double_t) integer + ((Double_t) decimal) * 1e-9;
}


/** */
ValErrPair UnPackDecimal(Double_t d)
{
   Int_t integer =  (Int_t) d;
   Double_t decimal = d - integer;

   ValErrPair ve(integer*1e-8, decimal*1e1);

   return ve;
}


/** */
TEllipse* GetEllipse(TVector2 xy1, TVector2 xy2, Double_t x_cntr, Double_t y_cntr, Double_t theta)
{
   TVector2 xy_c(x_cntr, y_cntr);
	xy1 -= xy_c;
	xy2 -= xy_c;

   xy1.Print();
   xy2.Print();

   xy1 = xy1.Rotate(theta*TMath::Pi()/180.);
   xy2 = xy2.Rotate(theta*TMath::Pi()/180.);
   xy1.Print();
   xy2.Print();


	Double_t numer  = xy1.X()*xy1.X()*xy2.Y()*xy2.Y() - xy2.X()*xy2.X()*xy1.Y()*xy1.Y();
	Double_t denom1 = xy2.Y()*xy2.Y() - xy1.Y()*xy1.Y();
	Double_t denom2 = xy1.X()*xy1.X() - xy2.X()*xy2.X();

   printf("test\n");
   
   xy1.Print();
   xy2.Print();

   printf(" numer, denom1, denom2: %lf, %lf, %lf\n", numer, denom1, denom2);
   
   if  (!denom1 || !denom2) return 0;

   Double_t a2 = numer / denom1;
   Double_t b2 = numer / denom2;

   if  (a2 < 0 || b2 < 0) return 0;

   return new TEllipse(x_cntr, y_cntr, TMath::Sqrt(a2), TMath::Sqrt(b2), 0, 360, theta);
}


/** */
TEllipse* GetErrorEllipse(const ValErrPair &p1, const ValErrPair &p2, Double_t corr)
{
   if (p1.e < 0 || p2.e < 0) return 0;

   Double_t theta = 0.5*TMath::ATan2(2*corr*p1.e*p2.e, p2.e*p2.e - p1.e*p1.e);
   //printf("theta: %f\n", theta);

   Double_t x1 =  p1.e*corr;
   Double_t y1 = -p2.e;

   Double_t x2 = -p1.e;
   Double_t y2 =  p2.e*corr;

   //Double_t numer  = x2*x2*y1*y1 - x1*x1*y2*y2;
   //Double_t denom1 = y1*y1 - y2*y2;
   //Double_t denom2 = x2*x2 - x1*x1;

   Double_t numer  = (x2*y1 - x1*y2) * ( (x2*y1 + x1*y2)*TMath::Cos(2*theta) + (y1*y2 - x1*x2)*TMath::Sin(2*theta));

   Double_t denom1 = (x1*x1 - x2*x2) * TMath::Sin(theta)*TMath::Sin(theta) +
                     (y1*y1 - y2*y2) * TMath::Cos(theta)*TMath::Cos(theta) +
                     (x2*y2 - x1*y1) * TMath::Sin(2*theta);

   Double_t denom2 = (x2*x2 - x1*x1) * TMath::Cos(theta)*TMath::Cos(theta) +
                     (y2*y2 - y1*y1) * TMath::Sin(theta)*TMath::Sin(theta) +
                     (x2*y2 - x1*y1) * TMath::Sin(2*theta);

   Double_t a = TMath::Sqrt(numer) / TMath::Sqrt(denom1);
   Double_t b = TMath::Sqrt(numer) / TMath::Sqrt(denom2);

   //if  (a2 < 0 || b2 < 0) return 0;

   return new TEllipse(p1.v, p2.v, a, b, 0, 360, 180-180.*theta/TMath::Pi());
}


/** */
void SetXAxisIntBinsLabels(TH1* h, Int_t xmin, Int_t xmax, Float_t tfx, Int_t ny)
{
   Int_t nMaxDivs = 50;

   Int_t nFillsPerDiv = ceil( float(xmax-xmin)/nMaxDivs );
   nFillsPerDiv = nFillsPerDiv < 1 ? 1 : nFillsPerDiv;

   Int_t nDivs = ceil( float(xmax-xmin) / nFillsPerDiv );

   xmax = xmin + nDivs*nFillsPerDiv;

   h->SetBins(nDivs, xmin, xmax, 1, -0.5, +0.5);

   //printf("xxx: %s, %d, %d, %d, %d\n", h->GetName(), xmin, xmax, nFillsPerDiv, nDivs);

   Int_t nSparse = 1;

   TAxis* xAxis = h->GetXaxis();
   xAxis->SetNdivisions(nDivs, kFALSE);

   Int_t xFill = xmin;

   for (int ib=1; ib<=xAxis->GetNbins(); ib++, xFill+=nFillsPerDiv )//, xmin+=nFillsPerDiv)
   {  
      //if ( xmin % nSparse != 0 ) continue;

      stringstream ssBinLabel(" ");

      //if ( ib-1 % roundUp == 0 )
      //if ( (xmin + ib - 1) % nSparse == 0 )
      if ( xFill % nSparse == 0 )
      {
         ssBinLabel << xFill;
         //xmin+=roundUp;
      }

      xAxis->SetBinLabel(ib, ssBinLabel.str().c_str());
   }

   h->LabelsOption("v");
   xAxis->CenterLabels(kTRUE);
}


/** */
void SetXYAxisIntBinsLabels(TH1* h, Int_t xmin, Int_t xmax, Int_t ymin, Int_t ymax, Float_t tfx)
{
   Int_t nx = xmax - xmin + 1;
   Int_t ny = ymax - ymin + 1;
   h->SetBins(nx, xmin-0.5, xmax+0.5, ny+1, ymin-0.5, ymax+0.5);

   Int_t nLabels = Int_t(nx*tfx); // tfx is tick frequency
   //h->SetNdivisions(10);
   Int_t nSparse = nLabels < 5 ? 1 : 5*Int_t(nLabels/5);

   TAxis* xAxis = h->GetXaxis();
   //xAxis->CenterLabels(kTRUE);

   for (int ib=1; ib<=xAxis->GetNbins(); ib++, xmin++)
   {  
      //Float_t remain = ((ib-1) % 2);
      //printf("ib, ib/, xmin: %d, %f, %f\n", ib, remain, xmin);
      //if ( (ib-1) % nLabels != 0) continue;
      if ( xmin % nSparse != 0 ) continue;

      stringstream ssBinLabel("");
      ssBinLabel << xmin;

      xAxis->SetBinLabel(ib, ssBinLabel.str().c_str());
   }

   h->LabelsOption("v");
   //xAxis->SetNdivisions(nx-1, kFALSE);
   //xAxis->SetNdivisions(nLabels, kFALSE);
}


}
