#! /bin/tcsh -f
foreach y (19 20 21)
    hadd COLGeV_20${y}.root  [0-9]*GeV_20${y}.root  [0-9]*GeV?_20${y}.root >&   COLGeV_20${y}.log &
    hadd FXT_20${y}.root  [0-9]*get_20${y}.root   >&   FXT_20${y}.log &
end
hadd 9p2GeVabc_2020.root 9p2GeV*2020.root >& 9p2GeVabc_2020.log &
