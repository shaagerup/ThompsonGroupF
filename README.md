ThompsonGroupF
==============

#### Prerequisites

Please 

- make sure that a recent version of sort from GNU coreutils is installed on the server.
- edit *runsortingalgpar.sh* to refer to sort instead of gsort [sort is called gsort on Mac OS X]
- make sure that ppss is installed on the server [a program to run different tasks in parallel, and wait for completion]
- make sure that there is plenty of space in the /tmp/ directory of the server


#### Short guide

Create the executable by

	$ ./make.sh

Run the algorithm by

	time ./runsortingalgpar.sh N P

where *N* is the instance parameter, and *P* is the number of cores available [incl. hyperthreaded cores].

The desired number will be written on the screen like

	$ time ./runsortingalgpar.sh 20 8
	Okt 08 18:35:44:  
	Okt 08 18:35:44:  =========================================================
	Okt 08 18:35:44:                         |P|P|S|S|                         
	Okt 08 18:35:44:  Distributed Parallel Processing Shell Script vers. 2.97
	Okt 08 18:35:44:  =========================================================
	Okt 08 18:35:44:  Hostname:
	new-host.home
	Okt 08 18:35:44:  ---------------------------------------------------------
	Okt 08 18:35:44:  CPU:  Intel Core i7  2,6 GHz
	Okt 08 18:35:44:  Found 8 logic processors.
	Okt 08 18:35:44:  Starting 8 parallel workers.
	Okt 08 18:35:44:  ---------------------------------------------------------
	Okt 08 18:35:52:  100% complete. Processed 8 of 8. Failed 0/8.               
	Okt 08 18:35:52:  Total processing time (hh:mm:ss): 00:00:08
	Okt 08 18:35:52:  Finished. Consult ppss_dir/job_log for job output.
	3310592

	real 0m36.016s
	user 2m28.214s
	sys 0m3.407s

From the above, we conclude that for N=20, the desired number is 3310592.


#### Longer guide

**DRAFT!**

If we want to split the execution between *S* servers we generate a task file for each server [pseudo-bash written below]

	for (( s=0; s<=S-1; p++ ))
	do
		for (( p=0; p<=P-1; p++ ))
		do
			echo "$1_$2_{$s*P + $p}" >> /tmp/tgtasks_$1_$s
		done
	done


On each server, use the task file to run the algorithm

	ppss -f /tmp/tgtasks_$1_$s -c './src/newSortingAlgPar "$ITEM" > /tmp/tg"$ITEM"'

Do a sort of the resulting files.

Merge all the sorted files from each server using -m parameter of GNU sort [see example in *runsortingalgpar_alternative.sh*].

