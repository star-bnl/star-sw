{

 // void star(float diameter)
// float diameter = 10;
float D[] = {0.5,0.2};
float d2rad =  asin(1)/90.;
float fStep = d2rad*360./10;
float f0    = 90.*d2rad - 2*fStep;
D[1] = D[0]*cos(2*fStep)/cos(fStep);

TCanvas cc("STARC","STARC",0,0,400,400);
TPad pad("STAR","STAR",0,0,1,1);
pad.Draw();
pad.cd();
TPolyLine sLine(10);
double d;
int id = 0;
int j = 0;
for (j=0;j<10;j++,f0+=fStep){
 d = D[id];
 sLine.SetPoint(j,d*cos(f0)+0.5,d*sin(f0)+0.45);
 if (id) id = 0;
 else id = 1;
}
sLine.SetFillColor(19);
sLine.SetLineColor(2);
sLine.SetLineWidth(7);
sLine.Draw();
TLatex *text = new TLatex(0.49,0.43,"STAR");
text->SetTextColor(2);
text->SetTextFont(52);
text->SetTextSize(0.2);
text->SetLineWidth(4);
text->Draw();
pad->Modified();
cc.Update();
}
