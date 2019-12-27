# Setup directory structure for timing

foreach vers (dev .DEV2 TFG19m)
  echo "vers = ${vers}"
  foreach bits (32b 64b)
    echo "bits = ${bits}"
    foreach deb (debug opt)
      echo "deb = ${deb}"
      set RC_list = "StiCA"
      set GCC_list = "485"
      if ("$vers" != "dev") then
        set RC_list = "StiCA Stx"
        set GCC_list = "485 631 731 830"
      endif
      foreach rc ($RC_list) 
        echo "rc = ${rc}"
        foreach comp ($GCC_list)
	  echo "comp = ${comp}"
          set dir = RC_${comp}_${bits}_${rc}_${vers}_${deb}
	  echo "dir = ${dir}"
          if (! -d ${dir}) mkdir ${dir}
	  cd ${dir}
          SetVersion
	  cd -
        endif
      end
    end
  end
end
