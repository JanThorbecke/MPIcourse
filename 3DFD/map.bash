#!/bin/bash
  
nmpi=16
~jthorbecke/tools/mpitrace/util/readsparse send_bytes.sparse.883001 | grep "sent to rank" | awk '{print $2 " "$6 }' > matrix.txt

for col in `seq 0 $nmpi`; do
   for row in `seq 0 $nmpi`; do
            echo -n "0 "
    done
    echo 
done

