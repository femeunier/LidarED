#!/bin/bash -l

# redirect output
exec 3>&1
exec &>> "/home/carya/R/LidarED/runs/out/ENS-00001-99000000406/logfile.txt"

TIMESTAMP=`date +%Y/%m/%d_%H:%M:%S`
echo "Logging on "$TIMESTAMP

# host specific setup


# create output folder
mkdir -p "/home/carya/R/LidarED/runs/out/ENS-00001-99000000406"
# no need to mkdir for scratch

# @REMOVE_HISTXML@ : tag to remove "history.xml" on remote for restarts, commented out on purpose


# flag needed for ubuntu
export GFORTRAN_UNBUFFERED_PRECONNECTED=yes

# see if application needs running
if [ ! -e "/home/carya/R/LidarED/runs/out/ENS-00001-99000000406/history.xml" ]; then
  cd "/home/carya/R/LidarED/runs/run/ENS-00001-99000000406"
  
  "/home/carya/ED2/ED/run/ed_2.1-opt" ""
  STATUS=$?
  if [ $STATUS == 0 ]; then
    if grep -Fq '=== Time integration ends; Total elapsed time=' "/home/carya/R/LidarED/runs/out/ENS-00001-99000000406/logfile.txt"; then
      STATUS=0
    else
      STATUS=1
    fi
  fi
  
  # copy scratch if needed
  # no need to copy from scratch
  # no need to clear scratch

  # check the status
  if [ $STATUS -ne 0 ]; then
  	echo -e "ERROR IN MODEL RUN\nLogfile is located at '/home/carya/R/LidarED/runs/out/ENS-00001-99000000406/logfile.txt'"
  	echo "************************************************* End Log $TIMESTAMP"
    echo ""
  	exit $STATUS
  fi

  # convert to MsTMIP
  echo "require (PEcAn.ED2)
model2netcdf.ED2('/home/carya/R/LidarED/runs/out/ENS-00001-99000000406', 51.7750579, -1.33904729, '2004/01/01', '2004/12/31', c('PFT_BCI_hydro_Late'))
" | R --vanilla
fi

# copy readme with specs to output
cp  "/home/carya/R/LidarED/runs/run/ENS-00001-99000000406/README.txt" "/home/carya/R/LidarED/runs/out/ENS-00001-99000000406/README.txt"

# run getdata to extract right variables

# host specific teardown


# all done
echo -e "MODEL FINISHED\nLogfile is located at '/home/carya/R/LidarED/runs/out/ENS-00001-99000000406/logfile.txt'"
echo "************************************************* End Log $TIMESTAMP"
echo ""
