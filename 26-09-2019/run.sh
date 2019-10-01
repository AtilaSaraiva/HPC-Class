#!/bin/bash
#$ -cwd
#$ -j y
#$ -S /bin/bash
#$ -pe orte 3
#$ -N job
#$ -q all.q

#/usr/bin/time -f "%E" /opt/openmpi/bin/mpirun -v -np $NSLOTS ./ex_firstmpi

#/usr/bin/time -f "%E" /opt/openmpi/bin/mpirun -v -np $NSLOTS ./montecarloparalelo

#/usr/bin/time -f "%E" /opt/openmpi/bin/mpirun -v -np $NSLOTS ./env_rec_msg_ex1

#/usr/bin/time -f "%E" /opt/openmpi/bin/mpirun -v -np $NSLOTS ./env_rec_msg_ex2

#/usr/bin/time -f "%E" /opt/openmpi/bin/mpirun -v -np $NSLOTS ./env_rec_msg_ex3

#/usr/bin/time -f "%E" /opt/openmpi/bin/mpirun -v -np $NSLOTS ./env_rec_msg_ex4

#/usr/bin/time -f "%E" /opt/openmpi/bin/mpirun -v -np $NSLOTS ./env_rec_msg_ex5

#/usr/bin/time -f "%E" /opt/openmpi/bin/mpirun -v -np $NSLOTS ./env_rec_msg_ex6

#/usr/bin/time -f "%E" /opt/openmpi/bin/mpirun -v -np $NSLOTS ./hello

/usr/bin/time -f "%E" /opt/openmpi/bin/mpirun -v -np $NSLOTS ./rtm_prestack_tiros
