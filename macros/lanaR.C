/*
  root.exe -q -b 'lanaR.C("StEvent/st_laser_adc_14044071_raw.ZF.Fit.g3.LandauIFreQ.event.root")'
  root.exe -q -b 'lanaR.C("StEvent/st_laser_adc_14046081_raw.F.Fit.g3.LandauIFreQ.event.root")'
  root.exe -q -b 'lanaR.C("StEvent/st_laser_adc_14072106_raw.RF.Fit.g3.LandauIFreQ.event.root")'
  root.exe -q -b 'lanaR.C("StEvent/st_laser_adc_14158028.raw.RF.Fit.g3.LandauIFreQ.event.root")'
  root.exe -q -b 'lanaR.C("StEvent/st_laser_adc_14161023.raw.ZF.Fit.g3.LandauIFreQ.event.root")'
 */
void lanaR(const Char_t *infile = "") {
  gROOT->LoadMacro("bfc.C");
  //  bfc(0,"MakeEvent,in,TpcHitMover,ITTF,LaserIT,VFMinuit,Lana,Analysis,Corr4,NosvtIT,NossdIT,StiPulls,NoHistos,NoRunco",infile);
  //    bfc(0,"MakeEvent,in,TpcHitMover,ITTF,LaserIT,KFVertex,Lana,Analysis,Corr4,NosvtIT,NossdIT,StiPulls,NoHistos,NoRunco",infile);
  //  bfc(0,"MakeEvent,in,TpcHitMover,ITTF,LaserIT,KFVertex,Lana,Analysis,NosvtIT,NossdIT,StiPulls,NoHistos,NoRunco,StiCA",infile);
  //  bfc(0,"MakeEvent,in,TpcHitMover,ITTF,LaserIT,KFVertex,Lana,Analysis,NosvtIT,NossdIT,StiPulls,NoHistos,NoRunco,StiCA,Corr4",infile);
  //  bfc(9999,"MakeEvent,in,TpxRaw,TpxClu,TpcHitMover,LaserIT,KFVertex,Lana,Analysis,NosvtIT,NossdIT,StiPulls,NoHistos,NoRunco,StiCA,Corr4,OSectorAlign",infile);
  //  bfc(1,"MakeEvent,in,TpxRaw,TpxClu,TpcHitMover,ITTF,LaserIT,VFMinuit,Lana,Analysis,NosvtIT,NossdIT,StiPulls,NoHistos,NoRunco,StiCA,Corr4,evout",infile);
  //  bfc(9999,"MakeEvent,in,TpcHitMover,ITTF,LaserIT,VFMinuit,LanaDVtpx,Analysis,NosvtIT,NossdIT,StiPulls,NoHistos,NoRunco,StiCA,Corr4",infile);
  //  bfc(0,"MakeEvent,in,TpcHitMover,ITTF,LaserIT,KFVertex,Lana,Analysis,Corr4,NosvtIT,NossdIT,StiPulls,NoHistos,NoRunco",infile);
  //  St_trgTimeOffsetC::instance()->SetLaser(kTRUE);
  //  chain->MakeEvent();
   bfc(200,"MakeEvent,in,Tpx,TpcHitMover,ITTF,LaserIT,VFMinuit,Lana,Analysis,NosvtIT,NossdIT,StiPulls,NoHistos,NoRunco,StiCA,Corr4,OSectorAlign",infile);
}
