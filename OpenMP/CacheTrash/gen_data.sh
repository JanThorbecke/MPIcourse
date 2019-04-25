#!/bin/bash

# Generate the data for the cache trash plots
# Things that can be varied in the script:
#
# NMAX:    The NMAX as defines in shared_cache.c
# load:    The number of iterations divided per thread
# max_num_threads:  We start from 1 thread only and go up to th number specified here
# max_repetitions:  Each experiment is repeated this amount of times in order to have
#                   more representative data and to weed out outliers.
# *_file:           The filenames of various intermediate files used in the process.
#
# The output consists fo files "nonshared.txt" and "shared.txt" that can be loaded
# in matlab for further processing.

ulimit -s hard
NMAX=512*1024
declare -a load=(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144)
declare -a load_captions=(1 2 4 8 16 32 64 128 256 512 1K 2K 4K 8K 16K 32K 64K 128K 256K)
max_num_threads=4
max_repetitions=10

temp_file="tmp.txt"
shared_file="shared.txt"
nonshared_file="nonshared.txt"
plotscript_file="plotscript.m"

### Things below this line (ideally) should not need any editing ###

# Check if shared_cache.c has been patched already
grep -q '^#include "load.h"' shared_cache.c
if [ $? -eq 0 ]; then
	echo "File shared_cache.c already patched."
else
	cp shared_cache.c shared_cache.c.orig
	cat shared_cache.c.orig | \
	sed '/^#include <assert.h>/ a\
#include "load.h"' | \
	sed "s/^#define NMAX 4096/#define NMAX $NMAX/" | \
	sed "s/schedule(static,1)/schedule(static,LOAD)/" > shared_cache.c
fi

# Remove previous results
rm -f $temp_file $shared_file $nonshared_file

# Run the program
num_loads=${#load[@]}
for (( I=0; I<$num_loads; I++ )); do
	echo "#define LOAD ${load[$I]}" > load.h
	make clean
	make
	for (( J=1; J<=$max_num_threads; J++ )); do
		for (( K=1; K<=$max_repetitions; K++ )); do
			echo Iters/thread: ${load[$I]}, num. threads: $J, repetition $K/$max_repetitions
			export OMP_NUM_THREADS=$J; ./shared_cache 2> $temp_file
			grep '^non' $temp_file | cut -d ' ' -f 4 >> $nonshared_file
			grep '^shared' $temp_file | cut -d ' ' -f 5 >> $shared_file
		done
	done
done

# Remove temp file
rm -f $temp_file load.h

# Generate matlab script for plotting the data.

for (( I=0; I<${#load_captions[@]}; I++ )); do
	x_captions="$x_captions, '${load_captions[$I]}'"
done

plotscript="
clear;
close all;
shared = importdata('$shared_file');
nonshared = importdata('$nonshared_file');

shared = permute(reshape(shared, [$max_repetitions $max_num_threads $num_loads]), [2 3 1]);
nonshared = permute(reshape(nonshared, [$max_repetitions $max_num_threads $num_loads]), [2 3 1]);

shared_mean = median(shared, 3);
nonshared_mean = median(nonshared, 3);

extrema = [min(min(shared_mean)) max(max(shared_mean))];
x={$x_captions};
xt = [1:2:$num_loads];

figure(1); contourf(shared_mean); title('Shared cache');
figure(2); contourf(nonshared_mean); title('Non-shared cache');

for I = 1:2
	figure(I);
	caxis(extrema);
	colorbar;
	xlabel('static block-size');
	ylabel('Threads');
	set(gca, 'xtick', xt);
	set(gca, 'xticklabel', x(xt));
end
"

echo "$plotscript" > $plotscript_file

# We are done!
echo ""
echo "Done. Start matlab, change to the current directory and execute plotscript.m to generate the plots."
