//
// plot a Bethe Bloch user defined function
//

Double_t fitf(Double_t* x, Double_t* par)
{
    BetheBloch bb;
//     Double_t val = par[0]*bb(x[0]/.13956995) + par[1];
    Double_t val = par[0]*bb(x[0]) + par[1];
    return val;
    //return BetheBloch::operator()(x[0]);
}
void bbm() {
//     gSystem->Load("St_base");
//     gSystem->Load("StarClassLibrary");
//     gSystem->Load("StPhysUtil");
    TCanvas* canvas = new TCanvas("bbCanvas","Bethe Bloch",600,650); 
    gPad->SetLogy();
    gPad->SetLogx();
    TF1* bethebloch = new TF1("bethebloch",fitf,.45,.46,2);
    bethebloch->SetParameters(1e6,0);
    bethebloch->SetParNames("scale","offset");
    bethebloch->Draw();
}
