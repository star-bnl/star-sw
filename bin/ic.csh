foreach i (10 20)
  foreach j (10 20 30 40 50)
    if (! -d i${i}c${j}) mkdir i${i}c${j}
    cd i${i}c${j}
    tar xvf ../tar.tar
    subst BichselPPMIP='' 75=${i} 2000=${j} *.*;
    cd -
  end
end
