class StBFChain;
StBFChain *chain;

void writeMuDst(Int_t nevents  = 10000000, 
		//		const Char_t *MainFile="/star/rcf/test/new/daq_sl302/year_2005/CuCu200_HighTower/st_physics_6054016_raw_1020005.event.root") {
		const Char_t *MainFile="./st_zerobias_adc_11065007_raw_1570001.event.root") {
  gROOT->LoadMacro("bfc.C");
  bfc(-1,"in,StEvent,StDbT,CMuDST,analysis,tree,nodefault,ReadAll,NoHistos,noTags,noRunco,quiet",MainFile);
  if (nevents >= 0)   {
    chain->Init();
    if (nevents > 0) chain->EventLoop(1,nevents);
  }
}
//  foreach f ( `ls -1d */*/*event.root` )
/* 
 set d = `dirname ${f}`;
 set b = `basename ${f} .event.root`
 if (-r ${d}/${b}.MuDst.root) continue;
 cd ${d}
 root.exe -q -b 'writeMuDst.C(10000,"'${b}'.event.root")' >& ${b}.writeMuDst.log 
 cd -
 end

 */
