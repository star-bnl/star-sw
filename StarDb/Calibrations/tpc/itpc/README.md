copy from /RTS/conf/itpc/itpc_gains.txt* and $CVSROOT/online/RTS/src/ITPC_SUPPORT/itpc_gains.txt

 cvs log $STAR/online/RTS/src/ITPC_SUPPORT/itpc_gains.txt > itpc.log
 awk '{\
       if ($1 == "revision") printf("cvs co -r %s -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >",$2);\
       if ($1 == "Version") printf("itpc_gains.txt.%08i.%06i\n",$4,$5);\
       }' itpc.log | tee co.csh
csh -x co.csh

foreach tpx (`ls -1d tpx_gains.*`)
  foreach tpc (`ls -1d tpc_gain*`)
    diff -s ${tpx} ${tpc} >& /dev/null
    if ($?) continue;
    echo "${tpx} and ${tpc} are matched"
    mv ${tpc} ${tpc}.matched
    ln -s ${tpx} ${tpc}
    break;
  end
end
tpx_gains.txt and tpc_gains.txt.20211213.102452 are matched
tpx_gains.txt.02Nov20.1 and tpc_gains.txt.20200807.104403 are matched
tpx_gains.txt.04Nov16.1 and tpc_gains.txt.20160211.125600 are matched
tpx_gains.txt.11Jan19.1 and tpc_gains.txt.20190110.060009 are matched
tpx_gains.txt.12Sep19.1 and tpc_gains.txt.20190623.153905 are matched
tpx_gains.txt.23Oct18.1 and tpc_gains.txt.20180326.052426 are matched
tpx_gains.txt.Apr01_21.bak and tpc_gains.txt.20210226.061422 are matched
tpx_gains.txt.Apr15_15.bak and tpc_gains.txt.20150202.091555 are matched
tpx_gains.txt.Apr29_13.1.bak and tpc_gains.txt.20130429.120404 are matched
tpx_gains.txt.Apr29_13.2.bak and tpc_gains.txt.20130429.120550 are matched
tpx_gains.txt.Apr29_13.bak and tpc_gains.txt.20130327.110450 are matched
tpx_gains.txt.Aug07_20.1.bak and tpc_gains.txt.20200807.101659 are matched
tpx_gains.txt.Aug07_20.bak and tpc_gains.txt.20191216.060513 are matched
tpx_gains.txt.Aug15_15.bak and tpc_gains.txt.20150415.165009 are matched
tpx_gains.txt.Aug26_09.1.bak and tpc_gains.txt.20010101.000000 are matched
tpx_gains.txt.Aug26_09.2.bak and tpc_gains.txt.20093426.133454 are matched
tpx_gains.txt.Aug27_09.bak and tpc_gains.txt.20093526.133548 are matched
tpx_gains.txt.Aug29_09.1.bak and tpc_gains.txt.20090829.134056 are matched
tpx_gains.txt.Aug29_09.2.bak and tpc_gains.txt.20090829.134450 are matched
tpx_gains.txt.Aug29_09.bak and tpc_gains.txt.20090828.113844 are matched
tpx_gains.txt.Aug30_09.bak and tpc_gains.txt.20090829.140248 are matched
tpx_gains.txt.Dec01_11.bak and tpc_gains.txt.20111130.074804 are matched
tpx_gains.txt.Dec09_09.bak and tpc_gains.txt.20091111.022632 are matched
tpx_gains.txt.Dec10_09.1.bak and tpc_gains.txt.20091210.141015 are matched
tpx_gains.txt.Dec10_09.bak and tpc_gains.txt.20091209.141757 are matched
tpx_gains.txt.Dec13_21.bak and tpc_gains.txt.20211029.041901 are matched
tpx_gains.txt.Dec14_09.bak and tpc_gains.txt.20091210.144450 are matched
tpx_gains.txt.Dec16_19.1.bak and tpc_gains.txt.20191216.031341 are matched
tpx_gains.txt.Dec16_19.2.bak and tpc_gains.txt.20191216.055625 are matched
tpx_gains.txt.Dec16_19.bak and tpc_gains.txt.20191129.041911 are matched
tpx_gains.txt.Feb01_14.bak and tpc_gains.txt.20140131.121642 are matched
tpx_gains.txt.Feb02_15.bak and tpc_gains.txt.20150121.123646 are matched
tpx_gains.txt.Feb03_11.bak and tpc_gains.txt.20110126.143551 are matched
tpx_gains.txt.Feb04_11.1.bak and tpc_gains.txt.20110204.132452 are matched
tpx_gains.txt.Feb04_11.2.bak and tpc_gains.txt.20110204.141720 are matched
tpx_gains.txt.Feb04_11.bak and tpc_gains.txt.20110203.112539 are matched
tpx_gains.txt.Feb04_Sep01_2011 and tpc_gains.txt.20110204.142155 are matched
tpx_gains.txt.Feb05_19.1.bak and tpc_gains.txt.20190205.030024 are matched
tpx_gains.txt.Feb05_19.2.bak and tpc_gains.txt.20190205.030315 are matched
tpx_gains.txt.Feb05_19.bak and tpc_gains.txt.20190129.081912 are matched
tpx_gains.txt.Feb09_12.bak and tpc_gains.txt.20120123.134210 are matched
tpx_gains.txt.Feb11_16.1.bak and tpc_gains.txt.20160211.105248 are matched
tpx_gains.txt.Feb11_16.2.bak and tpc_gains.txt.20160211.110340 are matched
tpx_gains.txt.Feb11_16.3.bak and tpc_gains.txt.20160211.121636 are matched
tpx_gains.txt.Feb11_16.bak and tpc_gains.txt.20160114.082038 are matched
tpx_gains.txt.Feb12_10.bak and tpc_gains.txt.20100129.050853 are matched
tpx_gains.txt.Feb26_19.bak and tpc_gains.txt.20190205.030534 are matched
tpx_gains.txt.Feb26_21.bak and tpc_gains.txt.20210114.112304 are matched
tpx_gains.txt.Feb27_18.1.bak and tpc_gains.txt.20180227.122857 are matched
tpx_gains.txt.Feb27_18.bak and tpc_gains.txt.20171113.114052 are matched
tpx_gains.txt.Feb27_19.bak and tpc_gains.txt.20190226.062805 are matched
tpx_gains.txt.from21 and tpc_gains.txt.20210503.085129 are matched
tpx_gains.txt.Jan07_13.bak and tpc_gains.txt.20121001.143459 are matched
tpx_gains.txt.Jan07_16.bak and tpc_gains.txt.20150112.193641 are matched
tpx_gains.txt.Jan10_19.bak and tpc_gains.txt.20150112.193641 are matched
tpx_gains.txt.Jan13_15.1.bak and tpc_gains.txt.20150113.064031 are matched
tpx_gains.txt.Jan14_16.bak and tpc_gains.txt.20160107.114627 are matched
tpx_gains.txt.Jan14_21.bak and tpc_gains.txt.20150112.193641 are matched
tpx_gains.txt.Jan19_19.1.bak and tpc_gains.txt.20190119.080422 are matched
tpx_gains.txt.Jan19_19.2.bak and tpc_gains.txt.20190119.081038 are matched
tpx_gains.txt.Jan19_19.bak and tpc_gains.txt.20150112.193641 are matched
tpx_gains.txt.Jan21_15.1.bak and tpc_gains.txt.20150121.122644 are matched
tpx_gains.txt.Jan21_15.bak and tpc_gains.txt.20150113.073009 are matched
tpx_gains.txt.Jan23_12.bak and tpc_gains.txt.20111201.075406 are matched
tpx_gains.txt.Jan26_10.1.bak and tpc_gains.txt.20100126.144144 are matched
tpx_gains.txt.Jan26_10.bak and tpc_gains.txt.20091214.165624 are matched
tpx_gains.txt.Jan26_11.bak and tpc_gains.txt.20100914.033102 are matched
tpx_gains.txt.Jan28_17.1.bak and tpc_gains.txt.20170128.193322 are matched
tpx_gains.txt.Jan28_17.2.bak and tpc_gains.txt.20170128.193728 are matched
tpx_gains.txt.Jan29_10.bak and tpc_gains.txt.20100126.145326 are matched
tpx_gains.txt.Jan29_19.bak and tpc_gains.txt.20190119.082255 are matched
tpx_gains.txt.Jan31_14.bak and tpc_gains.txt.20131029.055015 are matched
tpx_gains.txt.Jun03_12.bak and tpc_gains.txt.20120308.094947 are matched
tpx_gains.txt.Jun23_19.1.bak and tpc_gains.txt.20190623.031712 are matched
tpx_gains.txt.Jun23_19.bak and tpc_gains.txt.20190227.033057 are matched
tpx_gains.txt.Mar08_12.bak and tpc_gains.txt.20120209.092729 are matched
tpx_gains.txt.Mar20_17 and tpc_gains.txt.20170320.075429 are matched
tpx_gains.txt.Mar20_17.1.bak and tpc_gains.txt.20170320.075111 are matched
tpx_gains.txt.Mar20_17.bak and tpc_gains.txt.20170128.200956 are matched
tpx_gains.txt.Mar26_18.bak and tpc_gains.txt.20180227.131934 are matched
tpx_gains.txt.Mar27_13.1.bak and tpc_gains.txt.20130327.110136 are matched
tpx_gains.txt.Mar27_13.bak and tpc_gains.txt.20130107.082939 are matched
tpx_gains.txt.May03_21.bak and tpc_gains.txt.20210401.085802 are matched
tpx_gains.txt.Nov09_09.1.bak and tpc_gains.txt.20091109.044832 are matched
tpx_gains.txt.Nov09_09.2.bak and tpc_gains.txt.20091109.045116 are matched
tpx_gains.txt.Nov09_09.3.bak and tpc_gains.txt.20091109.045446 are matched
tpx_gains.txt.Nov10_09.bak and tpc_gains.txt.20091109.050346 are matched
tpx_gains.txt.Nov11_09.1.bak and tpc_gains.txt.20091111.021120 are matched
tpx_gains.txt.Nov11_09.bak and tpc_gains.txt.20091110.032722 are matched
tpx_gains.txt.Nov13_17.bak and tpc_gains.txt.20170320.075429 are matched
tpx_gains.txt.Nov29_19.1.bak and tpc_gains.txt.20191129.035320 are matched
tpx_gains.txt.Nov29_19.2.bak and tpc_gains.txt.20191129.041003 are matched
tpx_gains.txt.Nov29_19.bak and tpc_gains.txt.20191030.045652 are matched
tpx_gains.txt.Nov30_11.1.bak and tpc_gains.txt.20111130.073930 are matched
tpx_gains.txt.Nov30_11.bak and tpc_gains.txt.20111007.081206 are matched
tpx_gains.txt.Oct01_12.bak and tpc_gains.txt.20120603.141239 are matched
tpx_gains.txt.Oct07_11.1.bak and tpc_gains.txt.20111007.080924 are matched
tpx_gains.txt.Oct07_11.bak and tpc_gains.txt.20110926.030907 are matched
tpx_gains.txt.Oct19_09.bak and tpc_gains.txt.20090830.161716 are matched
tpx_gains.txt.Oct29_21.1.bak and tpc_gains.txt.20211029.040810 are matched
tpx_gains.txt.Oct29_21.bak and tpc_gains.txt.20150112.193641 are matched
tpx_gains.txt.Oct30_19.1.bak and tpc_gains.txt.20191030.044030 are matched
tpx_gains.txt.SAVE and tpc_gains.txt.20211213.102452 are matched
tpx_gains.txt.Sep14_10.bak and tpc_gains.txt.20100212.063502 are matched
tpx_gains.txt.Sep26_11.bak and tpc_gains.txt.20110204.142155 are matched
tpx_gains.txt.until.21Nov14 and tpc_gains.txt.20140201.090438 are matched
tpx_gains.txt.until.26Oct13 and tpc_gains.txt.20130429.121034 are matched
